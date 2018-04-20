module Genetic where

import qualified Data.MultiSet as T
--import qualified Data.Set as T
import Data.Sequence as S
import Data.List
import System.Random
import Data.Foldable
import Debug.Trace

import LeaveOneOut
import Base (Weight, nAttr, Problem, Seed)

type Solution = S.Seq Weight
type Fitness = Double
type Instance = ([Value],Class)
type Class = Double
type Value = Double

data Individual = Individual
 { solution :: !Solution
 , fitness :: Fitness
 } deriving (Eq)
instance Ord Individual where
  compare a b = compare (fitness a) (fitness b)
instance Show Individual where
  show i = show $ fitness i
  
type Population = T.MultiSet Individual

objectiveFunction :: Problem -> Solution -> Double
objectiveFunction training w = objective training (toList w)

-- Operadores de mutación.
mutation :: Double -> Int -> Solution -> Solution
mutation epsilon indx =
  S.adjust (trunc . (+ epsilon)) indx
  where
    trunc x
      | x > 1 = 1 
      | x < 0 = 0 
      | otherwise = x 

-- Crossing operators
arithcross :: Solution -> Solution -> Solution
arithcross = S.zipWith (\x y -> (x + y) / 2.0)


-- Binary tournament
binaryTournament :: StdGen -> Population -> Individual
binaryTournament g popl
  | fitness a < fitness b = a
  | otherwise             = b
  where
    [x,y] = Data.List.take 2 $ randomRs (0,T.size popl-1) g
    a = T.elems popl !! x
    b = T.elems popl !! y

replaceWorstBy :: Individual -> Population -> Population
replaceWorstBy iv = T.insert iv . T.deleteMin


initialPopulation :: Problem -> Seed -> Population
initialPopulation training s = T.map randomIndv (T.fromList $ Data.List.take 30 $ randoms (mkStdGen s))
  where
    randomIndv n = Individual randomSol (objectiveFunction training randomSol)
      where
        randomSol = S.fromList $ Data.List.take (nAttr training) $ randomRs (0.0,1.0) (mkStdGen n)
