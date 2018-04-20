module Ageca where

import Data.Random.Normal
import Data.Ord
import Data.List
import System.Random
import Data.Foldable
import Debug.Trace

import LocalSearch (sigma)
import Base (Weight, Problem, Seed)
import TemplateMain
import Genetic

data Environment = Environment
  { population :: !Population
  , gen :: StdGen
  , globalSteps :: !Int
  , currentSol :: Solution
  , currentFit :: !Double
  }

evolutionaryStep :: Problem -> Environment -> Environment
evolutionaryStep training (Environment popl g step s f) =
  Environment npopl g' (step + 1) news newf
  where
    [r1,r2,r3] = take 3 $ randoms g 
    (epsilon, g') = normal' (0,sigma) g
    -- Selection
    parent1 = solution $ binaryTournament (mkStdGen r1) popl
    parent2 = solution $ binaryTournament (mkStdGen r2) popl
    -- Crossing
    son = arithcross parent1 parent2
    -- Mutation
    -- TODO: Probabilidad de mutación
    mson = mutation epsilon (mod r3 (length son)) son
    fitnessmson = objectiveFunction training mson
    -- Replacing
    npopl = replaceWorstBy (Individual mson fitnessmson) popl
    newf = if fitnessmson > f then fitnessmson else f
    news = if fitnessmson > f then mson else s

age :: (Problem -> Environment -> Environment) -> Seed -> Problem -> Solution 
age evStep s training = currentSol $ until
  (\env -> globalSteps env > 15000)
  (evStep training)
  initialEnvironment
  where
    initialPopl = initialPopulation training s
    initialEnvironment = Environment initialPopl (mkStdGen s) 0 initialSol initialFit
    initialSol = solution $ minimumBy (comparing fitness) initialPopl
    initialFit = fitness $ minimumBy (comparing fitness) initialPopl 

ageca :: Seed -> Problem -> [Weight]
ageca s p = toList $ age evolutionaryStep s p

main :: IO ()
main = templateMain ageca
 
