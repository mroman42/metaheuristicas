module Individual
  ( Individual
  , nAttrInd
  , fitness
  -- Generación aleatoria.
  , randomIndividual
  -- Operador de mutación.
  , mutation
  -- Operadores de cruce.
  , arithcross
  , blx
  )
where

import Data.Random.Normal
import Data.Sequence as S
import Data.List as L
import System.Random
import Data.Foldable
-- import Debug.Trace

import LeaveOneOut
import Base (Problem, nAttr)

type Weight = Double
type Solution = S.Seq Weight
type Fitness = Double
--type Instance = ([Value],Class)
--type Class = Double
--type Value = Double


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


-- | Cruza aritméticamente dos individuos.
arithcross :: Problem -> Individual -> Individual -> Individual
arithcross training a b = fromSolution training $
  S.zipWith (\x y -> (x + y) / 2.0) (solution a) (solution b)

-- | Muta una componente aleatoria de un individuo.
mutation :: Problem -> StdGen -> Individual -> Individual
mutation training g a = fromSolution training $
  S.adjust (trunc . (+ epsilon)) indx (solution a)
  where
    -- Sigma. Variación de la normal que produce la mutación.
    sigma = 0.3    
    -- Genera la mutación y el índice al que se aplicará.
    (epsilon,g') = normal' (0,sigma) g
    (indx,_) = next g'
    -- Trunca los pesos.
    trunc x
      | x > 1 = 1 
      | x < 0 = 0 
      | otherwise = x

-- | Cruce BLX
blx :: Problem -> StdGen -> Individual -> Individual -> Individual
blx training g a b = fromSolution training $
  operation <$> S.zip (S.fromList . L.take (nAttrInd a) $ randoms g) (S.zip (solution a) (solution b))
  where
    operation :: (Int, (Weight, Weight)) -> Weight
    operation (seed, (x,y)) = fst $ randomR interval $ mkStdGen seed
      where
        -- Constante alpha que regula la amplitud del intervalo.
        alpha = 0.3
        -- Cálculo del intervalo en el que está el resultado de la mutación.
        (cmin,cmax,ii) = (min x y, max x y, max x y - min x y)
        interval = (max 0 (cmin - ii * alpha) , min 1 (cmax + ii * alpha))


-- Genera un individuo aleatorio.
randomIndividual :: Problem -> StdGen -> Individual
randomIndividual training g = fromSolution training $ S.fromList $
  L.take (nAttr training) $ randomRs (0.0,1.0) g
