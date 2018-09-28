module Types where

  type Coord = (Int, Int)

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