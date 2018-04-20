module Aggblx where


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
import Ageblx (blx)
import Aggca (mutatePopulation)
import LocalSearch (mutation, sigma)
import LeaveOneOut (objective)
import TemplateMain
import Debug.Trace

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
    sons = map (uncurry (blx (fst $ next g))) $ take nSons $ pair $ map solution $ parents
      where
        pair (a:b:xs) = (a,b) : pair xs
        pair [a] = [(a,a)]
        pair [] = []
        
    -- Mutation and final population
    newpopl =
       replaceWorstBy (Individual s f) $ -- Includes the best one from the previous generation.
       mutatePopulation training g $
       foldr (replaceWorstBy . (\u -> Individual u $ objective training u)) parents sons
    
    -- Best
    news = solution $ maximumBy (comparing fitness) newpopl
    newf = fitness $ maximumBy (comparing fitness) newpopl
    (_,g') = split g


aggblx :: Seed -> Problem -> Solution
aggblx = age evolutionaryStep


main :: IO ()
main = templateMain aggblx
