module Ageca where

import Data.Random.Normal
import Data.Ord
import Data.List
import System.Random
import LocalSearch (mutation, sigma)
import Base (nAttr)
import LeaveOneOut
import TemplateMain

type Seed = Int
type Class = Double
type Value = Double
type Weight = Double
type Instance = ([Value],Class)
type Problem = [Instance]
type Solution = [Weight]
type Fitness = Double

data Individual = Individual
 { solution :: Solution
 , fitness :: Fitness
 }
type Population = [Individual]

arithcross :: Solution -> Solution -> Solution
arithcross = zipWith (\ x y -> (x + y) / 2.0)

binaryTournament :: Seed -> Population -> Solution
binaryTournament n popl = solution $ minimumBy (comparing fitness) (map (popl !!) [x,y])
  where
    g = mkStdGen n
    size = length popl
    [x,y] = take 2 $ randomRs (0,size-1) g

data Environment = Environment
  { population :: !Population
  , gen :: StdGen
  , globalSteps :: !Int
  , currentSol :: Solution
  , currentFit :: !Double
  }

replaceWorstBy :: Individual -> Population -> Population
replaceWorstBy iv popl =
  if worstFitness < fitness iv
    then map (\(x,i) -> if i /= worstIndex then x else iv) (zip popl [0..])
    else popl
  where
    worst :: (Individual, Int)
    worst = minimumBy (comparing (fitness . fst)) (zip popl [0..])
    worstIndex = snd worst
    worstFitness = fitness $ fst worst

evolutionaryStep :: Problem -> Environment -> Environment
evolutionaryStep training (Environment popl g step s f) = Environment npopl g' (step + 1) news newf
  where
    [r1,r2,r3] = take 3 $ randoms g
    (epsilon, g') = normal' (0,sigma) g
    -- Selection
    parent1 = binaryTournament r1 popl
    parent2 = binaryTournament r2 popl
    -- Crossing
    son = arithcross parent1 parent2
    -- Mutation
    -- TODO: Probabilidad de mutación
    mson = mutation epsilon r3 son
    fitnessmson = objective training mson
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
    initialEnvironment = Environment initialPopl (mkStdGen s) 0 initialSol initialFit
    initialSol = solution $ minimumBy (comparing fitness) initialPopl
    initialFit = fitness $ minimumBy (comparing fitness) initialPopl

    initialPopl :: Population
    initialPopl = take 30 $ map randomIndv (randoms (mkStdGen s))
      where
        randomIndv n = Individual randomSol (objective training randomSol)
          where
            randomSol = take (nAttr training) $ randomRs (0,1) (mkStdGen n)

ageca :: Seed -> Problem -> Solution
ageca = age evolutionaryStep

main :: IO ()
main = templateMain ageca
