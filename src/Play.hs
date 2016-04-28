module Main where

import Types
import PongGame
import GUI
import Utils

initialGame :: Gene -> PongGame
initialGame g =
  Game { ballLoc = (-10, 30)
       , ballVel = (150, -300)
       , p1 = UserPlayer False False (-25)
       , p2 = GenePlayer D g (-40)
       , result = NotFinished 
       }

movePaddle' :: PongGame -> PongGame
movePaddle' g = let a  = mayMoveUserDown (mayMoveUserUp (p1 g))
                    b  = moveGene (p2 g)
                in g { p1 = a, p2 = b }
  where
    mayMoveUserUp :: UserPlayer -> UserPlayer
    mayMoveUserUp p = if movingUp p && getPos p < upperBound
                        then setPos p (+3)
                        else p
    
    mayMoveUserDown :: UserPlayer -> UserPlayer
    mayMoveUserDown p = if movingDown p && getPos p > lowerBound
                          then setPos p (subtract 3)
                          else p

    moveGene :: GenePlayer -> GenePlayer
    moveGene p =
      if move p == U
         then if getPos p < upperBound then setPos p (+3)         else p
         else if getPos p > lowerBound then setPos p (subtract 3) else p

interpretGene' :: PongGame -> PongGame
interpretGene' game =
  let (bx, by) = normalize (ballLoc game)
      q = doubrant (ballVel game)
      m = gene (p2 game) !! (q*64 + bx*8 + by)
  in game { p2 = (p2 game) { move = m } }

update' :: Float -> PongGame -> PongGame
update' seconds game =
  case result game of
    NotFinished -> (checkFinish . movePaddle' . interpretGene' . paddleBounce . wallBounce . moveBall seconds) game
    _           -> game

main :: IO ()
main = do
  (_, g) <- readGene
  play window background fps (initialGame g) render handleKeys update'
