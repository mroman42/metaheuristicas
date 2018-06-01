module SimulatedAnnealing where

import System.Random
import Control.Monad.Random
import Control.Monad.Loops

import Base (Problem, Solution, Seed)
import Individual
import TemplateMain

type Temperature = Double

data Environment = Environment
  { evals :: !Int
  , temp :: !Temperature
  , itemp :: !Temperature
  , ftemp :: !Temperature
  , indv :: Individual
  , best :: Individual
  , m :: !Int
  }

initialEnvironment :: Problem -> Rand StdGen Environment
initialEnvironment training = do
  -- Initial solution
  initialSol <- randomIndividual training

  -- Initial temperature. May depend on the initial solution.
  let cs0 = fitness initialSol
  let phi = 0.3
  let mu = 0.3
  let initialTemp = mu * cs0 / (- (log phi))

  -- Parameters
  let maxNeighbours = 10 * length training
  
  -- Initial environment.
  return Environment
    { evals = 0
    , temp = initialTemp
    , itemp = initialTemp
    , ftemp = 0.001
    , indv = initialSol
    , best = initialSol
    , m = div 15000 maxNeighbours
    }


annealingStep :: Problem -> Environment -> Rand StdGen Environment
annealingStep training env = do
  -- Genera un vecino.
  let current = indv env
  neighbour <- mutation training current
  let diff = fitness neighbour - fitness current

  -- Decide si el vecino escogido es mejor.
  epsilon <- getRandomR (0,1)
  let k = 1
  let newInd = if diff < 0.0 || epsilon < exp (- diff / (k * temp env))
            then neighbour else current

  -- Actualiza la mejor solución hasta el momento.
  let newBest = max newInd (best env)

  -- Esquema de enfriamiento de Cauchy.
  let t = temp env
  let beta = (itemp env - ftemp env) / (fromIntegral (m env) * itemp env * ftemp env)
  let newt = t / (1 + beta * t)
  
  return Environment
    { evals = evals env + 1
    , temp = newt
    , itemp = itemp env
    , ftemp = itemp env
    , indv = newInd
    , best = newBest
    , m = m env
    }

simulatedAnnealing' :: Problem -> Rand StdGen Solution
simulatedAnnealing' training = do
  -- Estado inicial del algoritmo.
  ienv <- initialEnvironment training
  
  -- Proceso de enfriamiento.
  fenv <- iterateUntilM
    (\env -> evals env > 15000)
    (annealingStep training)
    ienv

  -- Devuelve la mejor solución al final.
  return $ toListSolution . best $ fenv

simulatedAnnealing :: Seed -> Problem -> Solution
simulatedAnnealing s training = evalRand (simulatedAnnealing' training) (mkStdGen s)


main :: IO ()
main = templateMain simulatedAnnealing
