module Ageblx where

import Data.Random.Normal
import Data.List
import Data.Ord
import System.Random

import Ageca
  ( Seed
  , Solution
  , Problem
  , Weight
  , Environment (Environment)
  , Individual (Individual, fitness, solution)
  , binaryTournament
  , replaceWorstBy
  , age
  )
import LocalSearch (mutation, sigma)
import LeaveOneOut (objective)
import TemplateMain

blx :: Seed -> Solution -> Solution -> Solution
blx s p q = map h $ zip3 (randoms (mkStdGen s)) p q
  where
    h :: (Seed, Weight, Weight) -> Weight
    h (n, a,b) = fst $ randomR interval (mkStdGen n)
      where
        alpha = 0.3
        cmin = min a b
        cmax = max a b
        ii = cmax - cmin
        interval = (max 0 (cmin - ii * alpha) , min 1 (cmax + ii * alpha))
                   
evolutionaryStep :: Problem -> Environment -> Environment
evolutionaryStep training (Environment popl g step s f) = Environment npopl g' (step + 2) news newf
  where
    [r1,r2,r3,r4,r5] = take 5 $ randoms g
    (epsilon, g') = normal' (0,sigma) g
    -- Selection
    parent1 = solution $ binaryTournament r1 popl
    parent2 = solution $ binaryTournament r2 popl
    -- Crossing
    son1 = blx r4 parent1 parent2
    son2 = blx r5 parent1 parent2
    -- Mutation
    -- TODO: Probabilidad de mutación
    mson1 = mutation epsilon r3 son1
    mson2 = mutation epsilon r3 son2
    fitmson1 = objective training mson1
    fitmson2 = objective training mson2   
    -- Replacing
    npopl =
      replaceWorstBy (Individual mson2 fitmson2) $
      replaceWorstBy (Individual mson1 fitmson1) popl
    newf = if max fitmson1 fitmson2 > f then max fitmson1 fitmson2 else f
    news = if max fitmson1 fitmson2 > f then solution $ maximumBy (comparing fitness)
        [Individual mson2 fitmson2, Individual mson1 fitmson1] else s

ageblx ::  Seed -> Problem -> Solution
ageblx = age evolutionaryStep

main :: IO ()
main = templateMain ageblx
