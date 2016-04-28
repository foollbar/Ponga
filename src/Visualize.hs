module Main where

import Types
import Simulation
import GUI
import Utils

main :: IO ()
main = do
  (g1, g2) <- readGene
  simulate window background fps (initialSimulation g1 g2) render (const progress)
