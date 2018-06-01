module DECurrent where


import System.Random
import Control.Monad.Random
import Control.Monad.Loops
import Data.List as L
import Data.Sequence as S

import System.Random.Shuffle

import Individual
import Population
import Base
import TemplateMain

cCR :: Double
cCR = 0.5

cF :: Double
cF = 0.5

data Environment = Environment
  { popl :: [Individual] -- Población inicial de 50 elementos
  , evals :: !Int
  , best :: Individual
  }

initialEnv :: Problem -> Rand StdGen Environment
initialEnv training = do
  population <- randomPopulation 50 training
  return Environment
    { popl = pToList population
    , evals = 0
    , best = bestOf population
    }
  
  

-- | Crossover from three parents and a base individual in the Random case.
crossoverRand :: Problem -> Individual -> Individual -> Individual -> Individual -> Rand StdGen Individual
crossoverRand training indv parent1 parent2 parent3 =
  fromSolution training <$>
    sequence (
      S.zipWith4 cross
        (solution indv)
        (solution parent1)
        (solution parent2)
        (solution parent3))
  where
    -- Trunca el peso entre 0 y 1.
    trunc :: Weight -> Weight
    trunc x
      | x > 1 = 1 
      | x < 0 = 0 
      | otherwise = x

    cross :: Double -> Double -> Double -> Double -> Rand StdGen Weight
    cross i x1 x2 b3 = do
      rand <- getRandomR (0,1)
      return $ if rand < cCR
        then trunc $ i + cF * (b3 - i) + cF * (x1 - x2)
        else i

differentialstep :: Problem -> Environment -> Rand StdGen Environment
differentialstep training env = do
  -- Crea la siguiente generación cruzando permutaciones aleatorias de
  -- la lista de elementos.
  let indvs = popl env
  parents1 <- shuffleM indvs
  parents2 <- shuffleM indvs
  let bestl = repeat (best env)
  offspring <- sequence $ L.zipWith4 (crossoverRand training) indvs parents1 parents2 bestl
  -- La nueva población contiene en cada elemento el mejor entre el
  -- offspring y el individuo.
  let newpopl = L.zipWith max indvs offspring
  return env
    { popl = newpopl
    , evals = evals env + L.length indvs
    , best = maximum newpopl 
    }

differentialevolution' :: Problem -> Rand StdGen Solution
differentialevolution' training = do
  ienv <- initialEnv training
  fenv <- iterateUntilM
    (\env -> evals env > 15000)
    (differentialstep training)
    ienv
  return $ toListSolution $ best fenv

differentialevolution :: Seed -> Problem -> Solution
differentialevolution s training = evalRand (differentialevolution' training) (mkStdGen s)

main :: IO ()
main = templateMain differentialevolution
