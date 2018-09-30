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
                    2 -> M.insert c Forward m
                    3 -> M.insert c TIntersection m
                    4 -> M.insert c XIntersection m
                    _ -> M.insert c UnknownTile m

  -- Second scan goes through result of first scan and determines correct turns in map.
  secondScan :: M.Map Coord Tile -> Coord -> M.Map Coord Tile
  secondScan = undefined

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