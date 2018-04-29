module Genetic where

import System.Random
import Control.Monad.Random
import Control.Monad.Loops
import Base (Problem, Solution, Seed)

import Population
import Individual

data Environment = Environment
  { population :: !Population
  , globalSteps :: !Int
  , bestIndividual :: Individual
  }

-- | Paso evolutivo en el modelo estacionario. Toma dos padres por
-- torneo binario y los cruza para crear un descendiente que se
-- incluye en la población, desplazando al peor actual.
-- Toma un operador de cruce como argumento.
stationaryStep :: (Problem -> Individual -> Individual -> Rand StdGen Individual)
  -> Problem -> Environment -> Rand StdGen Environment
stationaryStep cross training (Environment popl step _) = do
  -- Elige los padres
  parent1 <- binaryTournament popl
  parent2 <- binaryTournament popl
  -- Genera 2 hijos cruzando los padres y los muta.
  sons <- replicateM 2 $ cross training parent1 parent2
  mutatedSons <- sequence $ mutation training <$> sons
  -- Genera una nueva población incluyendo a estos hijos.
  let newPopl = foldr replaceWorstBy popl mutatedSons
  return $ Environment newPopl (step+1) (bestOf newPopl)

-- | Paso evolutivo en el modelo generacional.
generationalStep :: (Problem -> Individual -> Individual -> Rand StdGen Individual)
  -> Problem -> Environment -> Rand StdGen Environment
generationalStep cross training (Environment popl step best) = do
  -- Elige tantos padres como individuos hay en la población.
  parents <- replicateM (size popl) $ binaryTournament popl
  -- Cruza al 70% de los padres, emparejando a cada uno con el siguiente.
  let nSons = quot (size popl * quot 70 2) 100
  sons <- take nSons <$> mapM (uncurry (cross training)) (pairing parents)
  
  -- Genera una nueva población de padres incluyendo a estos hijos,
  -- mutándola aleatoriamente (0.001%) y volviendo a incluir al final
  -- al mejor de la generación anterior.
  newPopl <-
    replaceWorstBy best <$>
    mutatePopulationN (quot (nAttrPopl popl * size popl) 1000) training
    (foldr replaceWorstBy (pFromList parents) sons)
  return $ Environment newPopl (step+1) (bestOf newPopl)
  where
    pairing (a:b:xs) = (a,b) : pairing xs
    pairing [a] = [(a,a)]
    pairing [] = []

-- | Aplica un modelo evolutivo repetidamente sobre una solución
-- inicial aleatoria al problema.
ag :: (Problem -> Environment -> Rand StdGen Environment)
   -> Problem -> Rand StdGen Solution
ag evStep training = do
  -- Escoge una población inicial aleatoria.
  popl <- randomPopulation training
  let initialEnvironment = Environment popl 0 (bestOf popl)
  
  -- Itera evolutivamente sobre el entorno mientras no supere la cota
  -- de pasos establecida.
  finalEnv <- iterateUntilM
     (\env -> globalSteps env > 15000)
     (evStep training)
     initialEnvironment
     
  -- Devuelve la solución dada por el mejor individuo.
  return $ toListSolution . bestIndividual $ finalEnv


-- | Abstracción de un algoritmo genético
genetic ::
  (Problem -> Environment -> Rand StdGen Environment) ->
  (Seed -> Problem -> Solution)
genetic evStep s training = evalRand (ag evStep training) (mkStdGen s)
