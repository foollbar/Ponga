module GUI (window, background, fps, render, handleKeys, play, simulate) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Types
import PongGame

fps :: Int
fps = 60

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

render :: (Player a, Player b) => Game a b -> Picture
render game =
  pictures [ball, walls,
            mkPaddle rose (-width'/2 + 30) $ getPos (p1 game),
            mkPaddle orange (width'/2 - 30) $ getPos (p2 game)]
  where
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid (width'-30) 10

    wallColor = greyN 0.5
    walls = pictures [wall (height'/2), wall (-height'/2)]

    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)

setMovingUp :: Bool -> UserPlayer -> UserPlayer
setMovingUp b u = u { movingUp = b }

setMovingDown :: Bool -> UserPlayer -> UserPlayer
setMovingDown b u = u { movingDown = b }

handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'i') Down _ _) game = game { p1 = setMovingUp   True (p1 game) }
handleKeys (EventKey (Char 'k') Down _ _) game = game { p1 = setMovingDown True (p1 game) }
handleKeys (EventKey (Char 'i') Up _ _)   game = game { p1 = setMovingUp   False (p1 game) }
handleKeys (EventKey (Char 'k') Up _ _)   game = game { p1 = setMovingDown False (p1 game) }
handleKeys _ game = game

