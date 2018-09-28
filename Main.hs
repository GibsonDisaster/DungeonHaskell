module Main where
  import Graphics.Gloss.Interface.Pure.Game
  import Types

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
  renderGame g = Pictures [wallR, wallL, wallF, floor]
    where
      wallF = translate 0 0 $ scale 15 15 $ color white $ lineLoop [(-20, -5), (20, -5), (20, 50), (-20, 50)]
      wallR = translate 0 0 $ scale 15 15 $ color blue $ polygon [(-20, -5), (40, -50), (-100, -5), (-100, 40)]
      wallL = translate 0 0 $ scale 15 15 $ color blue $ polygon [(20, -5), (-40, -50), (100, -5), (100, 40)]
      floor = translate 0 0 $ scale 15 15 $ color green $ polygon [(-20, -5), (20, -5), (40, -50), (-40, -50)]

  handleEvent :: Event -> Game -> Game
  handleEvent e g = g

  updateGame :: Float -> Game -> Game
  updateGame f g = g

  main :: IO ()
  main = play FullScreen black 60 initGame renderGame handleEvent updateGame