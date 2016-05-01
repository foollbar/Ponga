 {-# LANGUAGE RecordWildCards #-}

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


boxWidth = width'/factor'
boxHeight = height'/factor'

grid :: Picture
grid = pictures (xs ++ ys) where
  xs :: [Picture]
  xs = map (\x -> translate (-width'/2 + x) 0 $ color gridColor $ rectangleSolid 1 height') px
  
  ys :: [Picture]
  ys = map (\y -> translate 0 (-height'/2 + y) $ color gridColor $ rectangleSolid width' 1) py
  
  px :: [Float]
  px = takeWhile (<= width') $ iterate (+ boxWidth) 0
  
  py :: [Float]
  py = takeWhile (<= height') $ iterate (+ boxHeight) 0
  
  gridColor = greyN 0.1

highlightBox :: (Float, Float) -> Picture
highlightBox p =
  let (x,y) = normalize p
      (x',y') = (fromIntegral x, fromIntegral y)
  in translate ((-width'/2 + boxWidth/2) + x'*boxWidth)
               ((-height'/2 + boxHeight/2) + y'*boxHeight) $
                 color red $ rectangleWire boxWidth boxHeight

render :: (Player a, Player b) => Game a b -> Picture
render Game{..} = pictures 
  [ ball, walls, grid, highlightBox ballLoc
  , mkPaddle rose (-width'/2 + 30) $ getPos p1
  , mkPaddle orange (width'/2 - 30) $ getPos p2
  ]
  where
    ball = uncurry translate ballLoc $ color ballColor $ circleSolid 10
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
handleKeys (EventKey (Char 'i') Down _ _) Game{..} = Game { p1 = setMovingUp   True p1, .. }
handleKeys (EventKey (Char 'k') Down _ _) Game{..} = Game { p1 = setMovingDown True p1, .. }
handleKeys (EventKey (Char 'i') Up _ _)   Game{..} = Game { p1 = setMovingUp   False p1, .. }
handleKeys (EventKey (Char 'k') Up _ _)   Game{..} = Game { p1 = setMovingDown False p1, .. }
handleKeys _ game = game

