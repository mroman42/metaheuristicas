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
  , localevals :: !Int
  , maxNeigh :: !Int
  , successes :: !Int
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
    , localevals = 0
    , maxNeigh = maxNeighbours
    , successes = 0
    , temp = initialTemp
    , itemp = initialTemp
    , ftemp = min 0.001 (0.01 * initialTemp)
    , indv = initialSol
    , best = initialSol
    , m = div 15000 maxNeighbours
    }


searchStep :: Problem -> Environment -> Rand StdGen Environment
searchStep training env = do
  -- Genera un vecino.
  let current = indv env
  neighbour <- mutation training current
  let diff = fitness current - fitness neighbour

  -- Decide si saltará. Lo hace cuando el vecino escogido es mejor o
  -- aleatoriamente según la temperatura.
  let k = 1
  epsilon <- getRandomR (0,1)
  let jump = diff < 0.0 || epsilon < exp (- diff / (k * temp env))

  return env
    -- Cuando salta, hace que se reinicie el contador de vecinos
    -- máximos.
    { evals = evals env + 1
    , localevals = localevals env + 1
    , successes = if jump then successes env + 1 else successes env
    , indv = if jump then neighbour else current
    }
  
annealingStep :: Problem -> Environment -> Rand StdGen Environment
annealingStep training ienv = do
  -- Da varios pasos de búsqueda local hasta que encuentre algo mejor,
  -- limitados por el número máximo de vecinos.
  fenv <- iterateUntilM
    (\env -> localevals env > maxNeigh env || evals env > 15000 || successes env > maxNeigh env `div` 1000)
    (searchStep training)
    ienv

  -- Esquema de enfriamiento de Cauchy.
  let t = temp fenv
  let beta = (itemp fenv - ftemp fenv) / (fromIntegral (m fenv) * itemp fenv * ftemp fenv)
  let newt = t / (1 + beta * t)
  
  return fenv
    { localevals = 0
    , temp = newt
    , best = max (indv fenv) (best fenv)
    , successes = 0
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
