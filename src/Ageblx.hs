module Ageblx where

import Data.Random.Normal
import Data.List
import Data.Ord
import System.Random
import Data.Foldable
import Debug.Trace

import Base (Weight, Problem, Seed)
import Ageca (age, Environment (Environment, population, gen, globalSteps, currentSol, currentFit))
import Genetic
import LocalSearch (sigma)
import LeaveOneOut (objective)
import TemplateMain


                   
evolutionaryStep :: Problem -> Environment -> Environment
evolutionaryStep training (Environment popl g step s f) = Environment npopl g' (step + 2) news newf
  where
    [r1,r2,r3,r4,r5] = take 5 $ randoms g
    (epsilon, g') = normal' (0,sigma) g
    -- Selection
    parent1 = solution $ binaryTournament (mkStdGen r1) popl
    parent2 = solution $ binaryTournament (mkStdGen r2) popl
    -- Crossing
    son1 = trace ("Parents: " ++ show parent1 ++ show parent2) $ blx r4 parent1 parent2
    son2 = blx r5 parent1 parent2
    -- Mutation
    -- TODO: Probabilidad de mutación
    mson1 = mutation epsilon (mod r3 (length son1)) son1
    mson2 = mutation epsilon (mod r3 (length son1)) son2
    fitmson1 = objectiveFunction training mson1
    fitmson2 = objectiveFunction training mson2   
    -- Replacing
    npopl =
      replaceWorstBy (Individual mson2 fitmson2) $
      replaceWorstBy (Individual mson1 fitmson1) popl
    newf = if max fitmson1 fitmson2 > f then max fitmson1 fitmson2 else f
    news = if max fitmson1 fitmson2 > f then solution $ maximumBy (comparing fitness)
        [Individual mson2 fitmson2, Individual mson1 fitmson1] else s

ageblx :: Seed -> Problem -> [Weight]
ageblx s p = toList $ age evolutionaryStep s p

main :: IO ()
main = templateMain ageblx
