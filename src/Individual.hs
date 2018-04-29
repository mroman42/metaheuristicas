module Individual
  ( Individual
  , nAttrInd
  , fitness
  , solution
  , toListSolution
  -- Generación aleatoria.
  , randomIndividual
  -- Operador de mutación.
  , mutation
  -- Operadores de cruce.
  , arithcross
  , arithcross'  
  , blx
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
  S.zipWith (\x y -> (x + y) / 2.0) (solution a) (solution b)

arithcross' :: Problem -> Individual -> Individual -> Rand StdGen Individual
arithcross' training a b = return $ arithcross training a b

-- | Muta una componente aleatoria de un individuo.
mutation :: Problem -> Individual -> Rand StdGen Individual
mutation training a = do
  epsilon <- getNormal (0,sigma)
  indx <- getRandomR (0,nAttrInd a-1)  
  return $ fromSolution training $ S.adjust (trunc . (+ epsilon)) indx (solution a)
  where
    -- Sigma. Variación de la normal que produce la mutación.
    sigma = 0.3    
    -- Trunca el peso después de mutarlo.
    trunc x
      | x > 1 = 1 
      | x < 0 = 0 
      | otherwise = x

-- | Cruce BLX
blx :: Problem -> Individual -> Individual -> Rand StdGen Individual
blx training a b = do
  c <- sequence $ S.zipWith blxw (solution a) (solution b)
  return $ fromSolution training c
  where
    blxw :: Weight -> Weight -> Rand StdGen Weight
    blxw x y = getRandomR interval
      where
        -- Constante alpha que regula la amplitud del intervalo.
        alpha = 0.3
        -- Intervalo en el que está el resultado de la mutación.
        (cmin,cmax,ii) = (min x y, max x y, max x y - min x y)
        interval = (max 0 (cmin - ii * alpha) , min 1 (cmax + ii * alpha))

-- Genera un individuo aleatorio.
randomIndividual :: Problem -> Rand StdGen Individual
randomIndividual training = do
  let nattr = nAttr training
  attrs <- replicateM nattr (getRandomR (0.0, 1.0))
  return $ fromSolution training $ S.fromList attrs
