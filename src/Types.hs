 {-# LANGUAGE Rank2Types #-}

module Types (Move(..), Gene, Population,
              Radius, Position, Player(..), GenePlayer(..), UserPlayer(..), Game(..), PongGame,
              GameResult(..), approxRate, approxRate') where

import System.Random (Random(..))

data Move = U | D deriving (Enum, Show, Read, Eq, Ord)
instance Random Move where
  random g = case randomR (0, 1) g of
               (r, g') -> (toEnum r, g')
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                      (r, g') -> (toEnum r, g')
type Gene = [Move]
type Population = [Gene]

type Radius = Float
type Position = (Float, Float)

data Game a b = Game
  { ballLoc :: (Float, Float)
  , ballVel :: (Float, Float)
  , p1 :: a 
  , p2 :: b
  , result :: GameResult
  }

type PongGame = Game UserPlayer GenePlayer

class Player a where
  getPos :: a -> Float
  setPos :: a -> (Float -> Float) -> a

data UserPlayer = UserPlayer
  { movingUp :: Bool
  , movingDown :: Bool
  , upos :: Float
  }
instance Player UserPlayer where
  getPos = upos
  setPos p f = p { upos = f (upos p) }

data GenePlayer = GenePlayer
  { move :: Move
  , gene :: Gene
  , gpos :: Float
  }
instance Player GenePlayer where
  getPos = gpos
  setPos p f = p { gpos = f (gpos p) }

data GameResult = Finished | NotFinished

approxRate :: Int
approxRate = 8

approxRate' :: Float
approxRate' = fromIntegral approxRate
