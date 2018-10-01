module Util where
  import qualified Data.Map as M
  import Data.Bifunctor
  import Types

  {-
    Character Codes for Tile Types
    Forward - |
    ForwardLeftTurn - J
    XIntersection - +
    TIntersection - T
    LeftTurn - <
    RightTurn - >
    ForwardRightTurn - L
    DeadEnd - D
  -}

  dec :: Num a => a -> a
  dec x = x - 1

  inc :: Num a => a -> a
  inc x = x + 1

  -- read map file and create basic template of layout
  readMap :: String -> Int -> Int -> [MapPoint]
  readMap "" _ _ = []
  readMap (x:xs) x' y = case x of
                         '\n' -> [] ++ readMap xs 1 (y+1)
                         'x' -> [((x', y), UnknownTile)] ++ readMap xs (x' + 1) y
                         _ -> [] ++ readMap xs (x' + 1) y

  getNeighbors :: M.Map Coord Tile -> Coord -> [MapPoint]
  getNeighbors m c = l ++ r ++ u ++ d
    where
      l = case M.lookup (bimap (\x -> x - 1) id c) m of
            Just m' -> [(bimap (\x -> x - 1) id c, m')]
            Nothing -> []
      r = case M.lookup (bimap (+ 1) id c) m of
            Just m' -> [(bimap (+ 1) id c, m')]
            Nothing -> []
      u = case M.lookup (bimap id (+ 1) c) m of
            Just m' -> [(bimap id (+ 1) c, m')]
            Nothing -> []
      d = case M.lookup (bimap id (\x -> x - 1) c) m of
            Just m' -> [(bimap id (\x -> x - 1) c, m')]
            Nothing -> []

  -- First scan inserts deadends, forwards, intersections into the map.
  firstScan :: M.Map Coord Tile -> Coord -> M.Map Coord Tile
  firstScan m c = case length (getNeighbors m c) of
                    0 -> M.insert c UnknownTile m
                    1 -> M.insert c DeadEnd m
                    2 -> M.insert c UnknownTurn m
                    3 -> M.insert c UnknownTurn m
                    4 -> M.insert c XIntersection m
                    _ -> M.insert c UnknownTile m

  -- Second scan goes through result of first scan and determines correct turns in map.
  {-
    Where mm is the tile to be determined
    tl tm tr
    ml mm mr
    bl bm br
  -}
  secondScan :: M.Map Coord Tile -> Coord -> (Coord, Tile)
  secondScan m c = if t == UnknownTurn then undefined else (c, t)
    where
      numOfNeighbors = getNeighbors m c
      tl = case M.lookup (bimap dec dec c) m of
            Just t' -> t'
            Nothing -> UnknownTile
      tm = case M.lookup (bimap id dec c) m of
            Just t' -> t'
            Nothing -> UnknownTile
      tr = case M.lookup (bimap inc dec c) m of
            Just t' -> t'
            Nothing -> UnknownTile
      m1 = case M.lookup (bimap dec id c) m of
            Just t' -> t'
            Nothing -> UnknownTile
      mr = case M.lookup (bimap dec dec c) m of
            Just t' -> t'
            Nothing -> UnknownTile
      mm = case M.lookup c m of
            Just t' -> t'
            Nothing -> UnknownTile

  {-
    This function creates the final Map used by the game.
    1) read map.
    2) inital scan that replaces Nulls with specific tile type.
    3) second scan that resolves UnknownTiles into specific directions.
  -}
  createMap :: String -> M.Map Coord Tile
  createMap s = simpleMap
    where
      simpleMap = M.fromList $ readMap s 1 1