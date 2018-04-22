module Aggca where

import Data.Random.Normal
import Data.List
import Data.Ord
import System.Random

import Debug.Trace
import Base (Weight, Problem, Seed)
import Genetic

import Ageca
  ( Environment (Environment)
  , age
  )
import LocalSearch (sigma)
import LeaveOneOut (objective)
import TemplateMain

mutatePopulation :: Problem -> StdGen -> Population -> Population
mutatePopulation training g popl = foldr apply popl list
  where
    (g1,g2) = split g
    size = length popl
    nattr = nattrp popl
    factor = 1000
    n = quot (nattr * size) factor
    
    idvList = randomRs (0,size-1) g1
    idxList = randomRs (0,nattr) g2
    epsilons = normals' (0,sigma) g
    list = take n $ zip3 idvList idxList epsilons

    apply :: (Int,Int,Double) -> Population -> Population
    apply (idv,idx,epsilon) = _ idv applyIdv
      where
        applyIdv :: Individual -> Individual
        applyIdv (Individual w _) = Individual (applySol w) (objective training (applySol w))
          where
            applySol :: Solution -> Solution
            applySol = mutation epsilon idx
    
    

evolutionaryStep :: Problem -> Environment -> Environment
evolutionaryStep training (Environment popl g step s f) =
  trace ("Step: " ++ show step)
  Environment newpopl g' (step + nSons) news newf
  where
    size = length popl
    
    -- Selection
    parents = take size $ map (`binaryTournament` popl) (randoms g)
    
    -- Crossing 70% of the parents
    nSons = quot (length parents * 35) 100
    sons :: [Solution]
    sons = map (uncurry arithcross) $ take nSons $ pair $ map solution parents
      where
        pair (a:b:xs) = (a,b) : pair xs
        pair [a] = [(a,a)]
        pair [] = []
        
    -- Mutación y población final
    newpopl =
       replaceWorstBy (Individual s f) $
       mutatePopulation training g $
       foldr (replaceWorstBy . (\u -> Individual u $ objective training u)) parents sons
    
    -- Best
    news = solution $ maximumBy (comparing fitness) newpopl
    newf = fitness $ maximumBy (comparing fitness) newpopl
    (_,g') = split g

aggca :: Seed -> Problem -> Solution
aggca = age evolutionaryStep

main :: IO ()
main = templateMain aggca
