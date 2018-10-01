module Main where
  import Graphics.Gloss.Game hiding (play)
  import Graphics.Gloss.Interface.Pure.Game
  import Types
  import Util

  initPlayer :: Player
  initPlayer = Player {
    pPos = (0, 0),
    pMaxHealth = 10,
    pHealth = 10,
    pMaxStam = 10,
    pStam = 10,
    pMaxMana = 10,
    pMana = 10
  }

  initGame :: Game
  initGame = Game {
    currentMap = "level1",
    player = initPlayer
  }

  renderGame :: Game -> Picture
  renderGame g = Pictures [wallR, wallL, wallF, floor, enemy]
    where
      wallF = translate 0 0 $ scale 15 15 $ color white $ lineLoop [(-20, -5), (20, -5), (20, 50), (-20, 50)]
      wallR = translate 0 0 $ scale 15 15 $ color red $ polygon [(20, -5), (40, -50), (80, 75), (20, 75)]
      wallL = translate 0 0 $ scale 15 15 $ color red $ polygon [(-20, -5), (-40, -50), (-80, 75), (-20, 75)]
      floor = translate 0 0 $ scale 15 15 $ color green $ polygon [(-20, -5), (20, -5), (40, -50), (-40, -50)]
      enemy = translate 10 10 $ scale 13 13 $ png "res/enemies/carrotnerd.png"

  handleEvent :: Event -> Game -> Game
  handleEvent e g = g

  updateGame :: Float -> Game -> Game
  updateGame f g = g

  main :: IO ()
  main = play FullScreen black 60 initGame renderGame handleEvent updateGame