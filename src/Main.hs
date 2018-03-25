module Main where

import Data.List
import Data.Random.Normal
import System.Random

-- DEPENDENCIAS
import Base






-- LOCAL SEARCH
-- truncateWeights :: [UnWeight] -> [Weight]
-- truncateWeights = map (\x -> if x > 1 then 1 else if x < 0 then 0 else x)

-- mutation :: Double -> Int -> Solution -> Solution
-- mutation sigma seed w = truncateWeights (zipWith (+) w (mkNormals' (0,sigma) seed))

-- objective :: Problem -> Solution -> Double
-- objective training w = fromIntegral hits / fromIntegral (length training)
--   where
--     hits :: Int
--     hits = sum $ oneOut (knnHit w) training

-- localsearchstep :: (RandomGen g) => g -> Int -> Problem -> Solution -> Maybe Solution
-- localsearchstep gen searchBound training w =
--     find (\w' -> objective training w < objective training w')
--     $ take searchBound
--     $ map (($ w) . mutation 0.3) (randoms gen)

-- localsearch :: (RandomGen g) => g -> Int -> Int -> Problem -> Solution -> Solution
-- localsearch gen iters searchBound training = acc gen iters
--   where
--     acc :: (RandomGen g) => g -> Int -> Solution -> Solution
--     acc _    0 s = s
--     acc gen' n s = case localsearchstep gen' searchBound training s of
--                      Just s' -> acc ((snd . next) gen') (n-1) s'
--                      Nothing -> s


-- localsearchMetaheuristic :: (RandomGen g) => g -> Int -> Problem -> Solution
-- localsearchMetaheuristic gen leng s = localsearch gen 100 100 s (replicate leng 0)

-- localSearch :: (RandomGen g) => g -> Problem -> Solution
-- localSearch gen dataset = localsearchMetaheuristic gen (nAttr dataset) dataset


main :: IO ()
main = return ()
