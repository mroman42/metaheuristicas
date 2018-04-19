module Aggca where

import Data.Random.Normal
import Data.List
import Data.Ord
import System.Random

import Ageca
  ( Seed
  , Solution
  , Problem
  , Population
  , Environment (Environment)
  , Individual (Individual, fitness, solution)
  , binaryTournament
  , replaceWorstBy
  , age
  , arithcross
  )
import LocalSearch (mutation, sigma)
import LeaveOneOut (objective)
import TemplateMain

update :: Int -> (a -> a) -> [a] -> [a]
update n f (y:ys)
  | n == 0    = f y : ys
  | otherwise = y   : update (n-1) f ys
update _ _ [] = error "Empty list"

mutatePopulation :: Problem -> StdGen -> Population -> Population
mutatePopulation training g popl = foldr apply popl list
  where
    size = length popl
    nattr = length (solution $ popl !! 1)
    factor = 1000
    n = quot (nattr * size) factor
    
    idvList = randomRs (0,size-1) g
    idxList = randomRs (0,nattr) g
    epsilons = normals' (0,sigma) g
    list = take n $ zip3 idvList idxList epsilons

    apply :: (Int,Int,Double) -> Population -> Population
    apply (idv,idx,epsilon) = update idv applyIdv
      where
        applyIdv :: Individual -> Individual
        applyIdv (Individual w _) = Individual (applySol w) (objective training (applySol w))
          where
            applySol :: Solution -> Solution
            applySol = mutation epsilon idx
    
    
       
evolutionaryStep :: Problem -> Environment -> Environment
evolutionaryStep training (Environment popl g step s f) = Environment newpopl g' (step + nSons) news newf
  where
    -- Selection
    parents = map (`binaryTournament` popl) (randoms g)
    
    -- Crossing 70% of the parents
    nSons = quot (length parents * 7) 10
    sons :: [Solution]
    sons = map (uncurry arithcross) $ take nSons $ pair parents
      where
        pair (a:b:xs) = (a,b) : pair xs
        pair [a] = [(a,a)]
        pair [] = []
        
    -- Mutation and final population
    newpopl =
       replaceWorstBy (Individual s f) $ -- Includes the best one from the previous generation.
       mutatePopulation training g $
       foldr (replaceWorstBy . (\u -> Individual u $ objective training u)) popl sons
    
    -- Best
    news = solution $ maximumBy (comparing fitness) newpopl
    newf = fitness $ maximumBy (comparing fitness) newpopl
    (_,g') = split g

aggca :: Seed -> Problem -> Solution
aggca = age evolutionaryStep

main :: IO ()
main = templateMain aggca
