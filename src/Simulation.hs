module Simulation where

import Types
import PongGame
import Utils

type Simulation = Game GenePlayer GenePlayer

initialSimulation :: Gene -> Gene -> Simulation
initialSimulation g1 g2 =
  Game { ballLoc = (-10, 30)
       , ballVel = (150, -300)
       , p1 = GenePlayer D g1 (-25)
       , p2 = GenePlayer D g2 (-40) 
       , result = NotFinished 
       }

movePaddle :: Simulation -> Simulation
movePaddle g = let a  = moveUp (p1 g)
                   a' = moveDown a
                   b  = moveUp (p2 g)
                   b' = moveDown b
               in g { p1 = a', p2 = b' }
  where
    moveUp p =
      if move p == U && getPos p < upperBound then setPos p (+3) else p
    moveDown p =
      if move p == D && getPos p > lowerBound then setPos p (subtract 3) else p

interpretGene :: Simulation -> Simulation
interpretGene game =
  let (bx, by) = normalize (ballLoc game)
      q  = doubrant (ballVel game)
      m1 = gene (p1 game) !! (q*(factor*factor) + bx*factor + by)
      m2 = gene (p2 game) !! (q*(factor*factor) + bx*factor + by)
  in game { p1 = (p1 game) { move = m1 }, p2 = (p2 game) { move = m2 } }

progress :: Float -> Simulation -> Simulation
progress seconds game = 
  case result game of
    NotFinished -> (checkFinish . movePaddle . interpretGene . paddleBounce . wallBounce . moveBall seconds) game
    _           -> game

match :: (Gene, Gene) -> (Gene, Gene, Float)
match (a,b) = go 0.0 (initialSimulation a b) where
  go seconds game =
    let game' = progress (1/60) game
     in if seconds < 100 
         then
           case result game' of
             NotFinished -> go (seconds + 1/60) game'
             _           -> (a, b, seconds)
         else (a, b, seconds)
