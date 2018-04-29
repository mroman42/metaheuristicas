module Genetic where

import Debug.Trace

import System.Random
import Control.Monad.Random
import Control.Monad.Loops
import Base (Problem, Solution, Seed)
 
import Population
import Individual

data Environment = Environment
  { population :: !Population
  , globalSteps :: !Int
  , generation :: !Int
  , bestIndividual :: Individual
  }

-- | Paso evolutivo en el modelo estacionario. Toma dos padres por
-- torneo binario y los cruza para crear un descendiente que se
-- incluye en la población, desplazando al peor actual.
-- Toma un operador de cruce como argumento.
stationaryStep :: (Problem -> Individual -> Individual -> Rand StdGen [Individual])
  -> Problem -> Environment -> Rand StdGen Environment
stationaryStep cross training (Environment popl step gen _) = trace ("Step: " ++ show step) $ do
  -- Elige varios padres, que serán 2 ó 4 dependiendo del operador de
  -- cruce que usemos. Específicamente, 2 en BLX y 4 en aritmético. Nótese
  -- que la elección entre 2 o 4 se hace según necesidad con evaluación perezosa.
  parents <- take 4 <$> binaryTournamentsWoReplace popl
  -- Genera 2 hijos cruzando los padres y los muta, para ello usará tantos
  -- padres como sean necesarios, lo que dependerá del operador de cruce.
  sons <- take 2 . concat <$> mapM (uncurry (cross training)) (pairing parents)
  mutatedSons <- sequence $ fmap (mutationGenProb training) sons
  traceM $ "Sons: " ++ show sons
  traceM $ "Parents: " ++ show parents
  
  -- Genera una nueva población haciendo competir a los hijos.
  let newPopl = replaceBy mutatedSons popl
  -- Cada iteración habrá realizado dos evaluaciones.
  return $ Environment newPopl (step+2) (gen+1) (bestOf newPopl)
  where
    pairing (a:b:xs) = (a,b) : pairing xs
    pairing [a] = [(a,a)]
    pairing [] = []

-- | Paso evolutivo en el modelo generacional.
generationalStep :: (Problem -> Individual -> Individual -> Rand StdGen [Individual])
  -> Problem -> Environment -> Rand StdGen Environment
generationalStep cross training (Environment popl step gen best) = trace ("Popl: " ++ show popl) $ do
  -- El 70% de la nueva población cruzará. Tomamos el valor de cruces
  -- constante para mantener la esperanza dada por la probabilidad.
  let nCross = quot (size popl * 70) 100
  let nMut = quot (nAttrPopl popl * size popl) 1000
  -- Elige padres que no cruzan y padres que cruzan.  Los segundos son
  -- reemplazados por sus hijos dependiendo del número de hijos que generen.
  parentsWtCross <- take nCross               <$> binaryTournamentsWoReplace popl
  parentsWoCross <- take (size popl - nCross) <$> binaryTournamentsWoReplace popl
  sons <- concat <$> mapM (uncurry (cross training)) (pairing parentsWtCross)
  let intPopl = pFromList $ take (size popl) $ parentsWoCross ++ sons ++ oddpos parentsWtCross
  
  -- Muta aleatoriamente (0.001% en esperanza) y vuelve a incluir
  -- al final al mejor de la generación anterior si no estuviera ya en
  -- la población.
  newPopl <- replaceWorstByIfNotMember best <$> mutatePopulationN nMut training intPopl
  return $ Environment newPopl (step + nCross + quot nMut 3) (gen+1) (bestOf newPopl)
  where
    pairing (a:b:xs) = (a,b) : pairing xs
    pairing [a] = [(a,a)]
    pairing [] = []
    oddpos (_:a:xs) = a : xs
    oddpos [_] = []
    oddpos [] = []

-- | Paso memético genérico
memeticStep :: Double -> Int -> Problem -> Environment -> Rand StdGen Environment
memeticStep prob n training env = do
  traceM $ "mejor: " ++ show (fitness $ bestIndividual env)
  -- Aplica búsqueda local sobre la población
  (newpopl, evals) <- poplLocalSearchBest prob n training (population env)
  -- Devuelve la nueva población e incrementa el contador de la
  -- función objetivo.
  return $ env
    {population = newpopl}
    {globalSteps = globalSteps env + evals}

-- | Paso memético que aplica búsqueda local sobre los mejores 10 cromosomas.
memeticStepBest :: Problem -> Environment -> Rand StdGen Environment
memeticStepBest = memeticStep 1 10

-- | Paso memético que aplica búsqueda local con probabilidad 0.1
-- sobre cada cromosoma.
memeticStepProb :: Problem -> Environment -> Rand StdGen Environment
memeticStepProb training env = memeticStep 0.1 (size $ population env) training env

-- | Paso memético que aplica búsqueda local sobre todos los cromosomas.
memeticStepAll :: Problem -> Environment -> Rand StdGen Environment
memeticStepAll training env = memeticStep 1.0 (size $ population env) training env


-- | Aplica un modelo evolutivo repetidamente sobre una solución
-- inicial aleatoria al problema.
ag :: (Problem -> Environment -> Rand StdGen Environment)
   -> Problem -> Rand StdGen Solution
ag evStep training = do
  -- Escoge una población inicial aleatoria.
  popl <- randomPopulation training
  let initialEnvironment = Environment popl 0 0 (bestOf popl)
  
  -- Itera evolutivamente sobre el entorno mientras no supere la cota
  -- de pasos establecida.
  finalEnv <- iterateUntilM
     (\env -> globalSteps env > 15000)
     (evStep training)
     initialEnvironment
     
  -- Devuelve la solución dada por el mejor individuo.
  return $ toListSolution . bestIndividual $ finalEnv


-- | Aplica un modelo evolutivo memético repetidamente sobre una
-- solución inicial aleatoria al problema.
am :: (Problem -> Environment -> Rand StdGen Environment)
   -> (Problem -> Environment -> Rand StdGen Environment)
   -> Problem -> Rand StdGen Solution
am evStep mStep training = do
  -- Escoge una población inicial aleatoria.
  popl <- randomPopulation training
  let initialEnvironment = Environment popl 0 0 (bestOf popl)
  
  -- Itera evolutivamente sobre el entorno mientras no supere la cota
  -- de pasos establecida.
  finalEnv <- iterateUntilM
     (\env -> globalSteps env > 15000)
     (\env ->
        -- Cada diez pasos, aplica lo indicado por el memético.
        if mod (generation env) 10 == 0
          then (evStep training >=> mStep training) env
          else  evStep training env)
     initialEnvironment
     
  -- Devuelve la solución dada por el mejor individuo.
  return $ toListSolution . bestIndividual $ finalEnv



-- | Abstracción de un algoritmo genético
genetic ::
  (Problem -> Environment -> Rand StdGen Environment) ->
  (Seed -> Problem -> Solution)
genetic evStep s training = evalRand (ag evStep training) (mkStdGen s)

-- | Abstracción de un algoritmo memético
memetic ::
  (Problem -> Environment -> Rand StdGen Environment) ->
  (Problem -> Environment -> Rand StdGen Environment) ->  
  (Seed -> Problem -> Solution)
memetic evStep mStep s training = evalRand (am evStep mStep training) (mkStdGen s)
