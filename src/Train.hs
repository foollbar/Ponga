module Main where

import System.Random (randomIO, randomRIO)
import Control.Applicative
import Control.Monad (replicateM, mapAndUnzipM)
import Data.List (minimumBy)

import PongGame
import Types
import Simulation
import Utils

import Debug.Trace

populationSize = 200

randomGene :: IO Gene
randomGene = replicateM (8*8*2) randomIO

randomPopulation :: Int -> IO Population
randomPopulation n = replicateM n randomGene

getFittest :: (Population, Population) -> (Gene, Gene, Float)
getFittest (a,b) =
  let mates = [(x,y) | x <- a, y <- b]
      matchResult = map match mates
      m = minimumBy (\(_, _,a) (_, _,b) -> compare b a) matchResult
  in trace (show $ (\(_, _, t) -> t) m) $ m

selectRandomGenes :: Int -> Population -> IO Population
selectRandomGenes n p = do
  ridx <- replicateM n (randomRIO (0, length p - 1))
  return (map (p !!) ridx)

selectPair :: (Population, Population) -> IO ((Gene, Gene), (Gene, Gene))
selectPair (a,b) = do
  p1 <- selectRandomGenes 5 a
  p2 <- selectRandomGenes 5 b
  
  p1' <- selectRandomGenes 5 a
  p2' <- selectRandomGenes 5 b

  let (g1, g2, _) = getFittest (p1, p2)
  let (g1', g2', _) = getFittest (p1', p2')
  return ((g1, g2), (g1', g2'))

mutate :: (Gene, Gene) -> IO (Gene, Gene)
mutate (a,b) = (,) <$> mapM mayMutate a <*> mapM mayMutate b where
  mayMutate :: Move -> IO Move
  mayMutate m = do
    p <- randomIO :: IO Float
    if p < 0.01 then randomIO else return m

crossOver :: ((Gene, Gene), (Gene, Gene)) -> IO (Gene, Gene)
crossOver ((a,b), (c,d)) = (,) <$> mapM cross (zip a c) <*> mapM cross (zip b d) where
  cross :: (Move, Move) -> IO Move
  cross (a,b) = do
    p <- randomIO :: IO Float
    return (if p < 0.5 then a else b)
                     
evolve :: (Population, Population) -> IO (Population, Population)
evolve p = mapAndUnzipM id $ replicate populationSize (selectPair p >>= crossOver >>= mutate)

loop :: Int -> (Population, Population) -> IO (Population, Population)
loop 0 p = return p
loop n p = do
  p' <- evolve p
  putStrLn $ "                    " ++ show n ++ "세대 남음"
  loop (n-1) p'

main :: IO ()
main = do
  pp1 <- randomPopulation populationSize
  pp2 <- randomPopulation populationSize
  (pp1', pp2') <- loop 40 (pp1, pp2)
  let (g1, g2, _) = getFittest (pp1', pp2')
  saveGene (g1, g2)
