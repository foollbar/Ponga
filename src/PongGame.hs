module PongGame where

import Types
import Utils

width, height, offset :: Int
width = 500
height = 500
offset = 100

width' = fromIntegral width
height' = fromIntegral height
offset' = fromIntegral offset

upperBound = (height' / 2) - 10 - 43
lowerBound = (-height' / 2) + 10 + 43

normalize :: (Float, Float) -> (Int, Int)
normalize (x,y) =
  ( round ((x + width'/2)  * (approxRate' - 1) / width')
  , round ((y + height'/2) * (approxRate' - 1) / height')
  )

moveBall :: Float -> Game a b -> Game a b
moveBall seconds game = game { ballLoc = (x', y') }
  where
   (x, y) = ballLoc game
   (vx, vy) = ballVel game

   x' = x + vx * seconds
   y' = y + vy * seconds

paddleBounce :: (Player a, Player b) => Game a b -> Game a b
paddleBounce game = game { ballVel = (vx', vy') } where
  p1y = getPos $ p1 game
  p2y = getPos $ p2 game
  (vx, vy) = ballVel game
  vx' = if paddleCollision1 (ballLoc game) p1y p2y
          then -vx
          else vx
  vy' = if paddleCollision2 (ballLoc game) p1y p2y
          then -vy
          else vy

wallBounce :: Game a b -> Game a b
wallBounce game = game { ballVel = (vx, vy') } where
  (vx, vy) = ballVel game
  vy' = if wallCollision (ballLoc game)
          then -vy
          else vy

wallCollision :: Position -> Bool
wallCollision (_, y) = topCollision || bottomCollision where
  topCollision = y >= height' / 2 - 15
  bottomCollision = y <= -height' / 2 + 15

p1x = (-width'/2)+30+13+3
p2x = (width'/2)-30-13-3

paddleCollision1 :: Position -> Float -> Float -> Bool
paddleCollision1 (x, y) p1y p2y = aCollision || bCollision where
  aCollision = inBound (p1x-3) p1x x && inBound (p1y - 43) (p1y + 43) y
  bCollision = inBound p2x (p2x+3) x && inBound (p2y - 43) (p2y + 43) y

paddleCollision2 :: Position -> Float -> Float -> Bool
paddleCollision2 (x, y) p1y p2y = aCollision || bCollision where
  aCollision = x <= p1x-3 && inBound (p1y - 43) (p1y + 43) y
  bCollision = x >= p2x+3 && inBound (p2y - 43) (p2y + 43) y

checkFinish :: Game a b -> Game a b
checkFinish g =
  let (x, y) = ballLoc g in
  if x < -(width'/2) + 10
    then g { result = GLeft (p1 g) }
    else if x > (width'/2) - 10
           then g { result = GRight (p2 g) }
           else g { result = NotFinished }

