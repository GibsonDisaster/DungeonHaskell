module Types where

  type Coord = (Int, Int)
  type MapPoint = (Coord, Tile)

  data Player = Player {
    pPos :: Coord,
    pMaxHealth :: Int,
    pHealth :: Int,
    pMaxStam :: Int,
    pStam :: Int,
    pMaxMana :: Int,
    pMana :: Int
  } deriving (Show, Eq, Ord)

  data Game = Game {
    currentMap :: String,
    player :: Player
  } deriving (Show, Eq, Ord)

  data LevelMap = LevelMap {
    points :: [MapPoint]
  }

  data Tile = Wall | Forward | ForwardLeftTurn | XIntersection | LeftTurn | RightTurn | ForwardRightTurn | TIntersection | UnknownTile | DeadEnd deriving (Show, Eq, Ord)