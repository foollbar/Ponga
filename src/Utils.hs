module Utils where

import Types
import Data.Maybe (fromJust)
import Data.List (elemIndex)

saveGene :: (Gene, Gene) -> IO ()
saveGene p = writeFile "gene.txt" (show p)

readGene :: IO (Gene, Gene)
readGene = fmap read (readFile "gene.txt")

inBound :: Float -> Float -> Float -> Bool
inBound from to target = from < target && target < to

doubrant :: (Float, Float) -> Int
doubrant (_,y) = if y > 0 then 0 else 1

diff :: Gene -> Gene -> Int
diff a b = length $ filter (uncurry (/=)) $ zip a b

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

maxIndex :: Ord a => [a] -> Int
maxIndex xs = fromJust $ elemIndex (maximum xs) xs

