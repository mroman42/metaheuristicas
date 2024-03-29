module Individual
  ( Individual
  , nAttrInd
  , fitness
  , solution
  , distance
  , toListSolution
  , fromSolution
  -- Generación aleatoria.
  , randomIndividual
  -- Operador de mutación.
  , mutation
  , mutationGenProb
  , strongMutation
  -- Búsqueda local
  , localSearchIn
  -- Operadores de cruce.
  , arithcross
  , arithcross'  
  , blx
  , blx'
  , blx''
  , twoint
  )
where

import Control.Monad.Random
import qualified Data.Sequence as S
import Data.Foldable
import Normal


import LeaveOneOut
import Base (Problem, nAttr)

type Weight = Double
type Solution = S.Seq Weight
type Fitness = Double

data Individual = Individual
 { solution :: !Solution
 , fitness :: Fitness
 } deriving (Eq)

instance Ord Individual where
  -- Comparamos dos individuos por su bondad.  Dos individuos nunca
  -- son iguales si son comparados por orden.
  compare a b
    | fitness a > fitness b = GT
    | fitness a < fitness b = LT
    | otherwise = GT
  
  
instance Show Individual where
  show i = show $ fitness i

-- | Tamaño de un individuo, número de atributos.
nAttrInd :: Individual -> Int
nAttrInd = S.length . solution

-- | Genera un individuo desde una solución. Necesita usar el conjunto
-- de entrenamiento para calcular su bondad.
fromSolution :: Problem -> Solution -> Individual
fromSolution training s = Individual s (objective training (toList s))

-- | Devuelve su solución en formato lista.
toListSolution :: Individual -> [Weight]
toListSolution = toList . solution

-- | Cruza aritméticamente dos individuos.
arithcross :: Problem -> Individual -> Individual -> Individual
arithcross training a b = fromSolution training $
  S.zipWith mean (solution a) (solution b)
  where
    mean x y = (x + y) / 2.0

-- | Versión general del cruce aritmético
arithcross' :: Problem -> Individual -> Individual -> Rand StdGen [Individual]
arithcross' training a b = return [arithcross training a b]

-- | Muta una componente aleatoria de un individuo.
mutation' :: Double -> Problem -> Individual -> Rand StdGen Individual
mutation' sigma training a = do
  epsilon <- getNormal (0,sigma)
  indx <- getRandomR (0,nAttrInd a-1)  
  return $ fromSolution training $ S.adjust (trunc . (+ epsilon)) indx (solution a)
  where
    -- Trunca el peso después de mutarlo.
    trunc x
      | x > 1 = 1 
      | x < 0 = 0 
      | otherwise = x

mutation :: Problem -> Individual -> Rand StdGen Individual
mutation = mutation' 0.3

-- | Sobre cada gen, aplicará una mutación aleatoria con probabilidad 0.001.
mutationGenProb :: Problem -> Individual -> Rand StdGen Individual
mutationGenProb training a = do
  n <- length . filter (< (0.001 :: Double)) . take (nAttrInd a) <$> getRandomRs (0.0,1.0)
  foldr (<=<) return (replicate n (mutation training)) a

-- | Mutate a component
mutateComponent :: Double -> Int -> Problem -> Individual -> Rand StdGen Individual
mutateComponent sigma indx training a = do
  epsilon <- getNormal (0, sigma)
  return $ fromSolution training $ S.adjust (trunc . (+ epsilon)) indx (solution a)
  where
    -- Trunca el peso después de mutarlo.
    trunc x
      | x > 1 = 1 
      | x < 0 = 0 
      | otherwise = x
  

-- | Aplica mutaciones sobre un 10% de las características; usa 0.4
-- como sigma.
strongMutation :: Problem -> Individual -> Rand StdGen Individual
strongMutation training a = do
  let t = div (nAttrInd a) 10
  indxs <- take t <$> getRandomRs (0, nAttrInd a-1)
  foldM (flip ($)) a (map (\i -> mutateComponent sigma i training) indxs)
  where
    sigma = 0.4

-- | Cruce BLX
blx :: Double -> Problem -> Individual -> Individual -> Rand StdGen Individual
blx alpha training a b = do
  c <- sequence $ S.zipWith blxw (solution a) (solution b)
  return $ fromSolution training c
  where
    blxw :: Weight -> Weight -> Rand StdGen Weight
    blxw x y = getRandomR interval
      where
        -- Intervalo en el que está el resultado de la mutación.
        (cmin,cmax,ii) = (min x y, max x y, max x y - min x y)
        interval = (max 0 (cmin - ii * alpha) , min 1 (cmax + ii * alpha))

-- | Versión general del cruce BLX
blx' :: Problem -> Individual -> Individual -> Rand StdGen [Individual]
blx' training a b = replicateM 2 (blx 0.3 training a b)

blx'' :: Problem -> Individual -> Individual -> Rand StdGen [Individual]
blx'' training a b = replicateM 2 (blx 0.5 training a b)


-- | Cruce con intervalo.
twoint :: Problem -> Individual -> Individual -> Rand StdGen [Individual]
twoint training a b = do
  u <- blx 0.3 training a b
  v <- randomIndividual training
  return [u,v]

-- | Genera un individuo aleatorio.
randomIndividual :: Problem -> Rand StdGen Individual
randomIndividual training = do
  let nattr = nAttr training
  attrs <- replicateM nattr (getRandomR (0.0, 1.0))
  return $ fromSolution training $ S.fromList attrs


-- Búsqueda Local Simplificada
-- | En un paso de búsqueda local, toma el mejor entre el actual y una
-- mutación suya.
explore :: Problem -> Individual -> Rand StdGen Individual
explore training a = max a <$> mutation training a

-- | Búsqueda local basada en un individuo con probabilidad
-- fija. Simplemente realiza un número dado de pasos de exploración.
-- Devuelve las evaluaciones que ha gastado.
localSearchIn :: Double -> Int -> Problem -> Individual -> Rand StdGen (Individual,Int)
localSearchIn prob n training indv = do
  epsilon <- getRandomR (0.0 , 1.0)
  if prob >= epsilon then do
    newindv <- foldr (>=>) return (replicate n (explore training)) indv
    return (newindv, n)
  else
    return (indv, 0)
  

distance :: Individual -> Individual -> Double
distance a b = sqrt $ sum $ (**2) <$> S.zipWith (-) (solution a) (solution b)
