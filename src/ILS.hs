module ILS where

import System.Random
import Control.Monad.Random
import Control.Monad.Loops

import Base (Problem, Solution, Seed)
import Individual
import TemplateMain


data Environment = Environment
  { evals :: !Int      -- Número de evaluaciones.
  , nsearchs :: !Int   -- Número de búsquedas locales ejecutadas.
  , best :: Individual -- Mejor solución encontrada
  , indv :: Individual -- Solución actual
  }

initialEnv :: Problem -> Rand StdGen Environment
initialEnv training = do
  -- Initial solution
  initialSol <- randomIndividual training
  return Environment
    { evals = 0
    , nsearchs = 0
    , best = initialSol
    , indv = initialSol
    }

localsearchstep :: Problem -> Environment -> Rand StdGen Environment
localsearchstep training env = do
  -- Genera un vecino, y elige el mejor entre ese vecino y el
  -- original.  Incrementa el contador de evaluaciones.
  neighbour <- mutation training $ indv env
  return $ env
    { evals = evals env + 1
    , indv = max neighbour (indv env)
    }

comparestep :: Problem -> Environment -> Rand StdGen Environment
comparestep training env = do
  -- Compara la nueva solución encontrada con la mejor hasta el
  -- momento.
  let newbest = max (best env) (indv env)
  
  -- Aplica una mutación fuerte sobre la mejor solución y sigue
  -- buscando.
  newbestmut <- strongMutation training newbest

  return $ env
    { evals = 0
    , nsearchs = nsearchs env + 1
    , best = newbest
    , indv = newbestmut
    }
  

ils' :: Problem -> Rand StdGen Solution
ils' training = do
  ienv <- initialEnv training
  fenv <- iterateUntilM
    (\env -> nsearchs env == 15)
    (\env ->
       if evals env > 1000
       then comparestep training env
       else localsearchstep training env)
    ienv
    
  return $ toListSolution $ best fenv


ils :: Seed -> Problem -> Solution
ils s training = evalRand (ils' training) (mkStdGen s)

main :: IO ()
main = templateMain ils
