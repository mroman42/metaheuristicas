module Base where

import Data.List
import Data.Ord
import Control.Arrow hiding (left, right)

-- Las instancias se clasifican en varias clases. Cada una de ellas
-- está representada por su vector de valores para cada atributo. Un
-- problema está dado por un conjunto de instancias clasificadas.
-- Tanto el Training como el Test son dos problemas.
type Class = Double
type Value = Double
type Instance = ([Value],Class)
type Problem = [Instance]
type Training = Problem
type Test = Problem
type Time = Double


-- Propiedades de los problemas.
-- | Número de atributos que tiene un problema.
nAttr :: Problem -> Int
nAttr [] = 0
nAttr ((a,_):_) = length a


-- Los pesos vienen dados por flotantes. Una solución es un vector
-- de pesos. Los pesos deben estar en el intervalo [0,1], mientras
-- que consideramos pesos no normalizados con otro tipo distinto.
type Weight = Double
type UnWeight = Double
type Solution = [Weight]

-- Una heurística toma un problema y devuelve una solución. Un
-- clasificador toma una instancia y le asigna una clase.
type Heuristic = Problem -> Solution
type Classifier = Instance -> Class

-- Las distancias vienen dadas en flotantes.
type Distance = Double

-- Las semillas de aleatoriedad son enteros.
type Seed = Int

-- Mide la distancia euclídea con pesos entre dos instancias.
-- Los pesos menores que 0.2 se descartan automáticamente.
-- Nótese que esta es la distancia al cuadrado.
{-# INLINE distSqrd #-}
distSqrd :: Solution -> Instance -> Instance -> Distance
distSqrd v x y = distSqrd' v (fst x) (fst y)
  where
    distSqrd' :: [Weight] -> [Value] -> [Value] -> Distance
    distSqrd' w a b = sum $ zipWith (*) w' $ map (** 2.0) (zipWith (-) a b)
      where
        w' :: [Weight]
        w' = map (\z -> if z < 0.2 then 0 else z) w    

-- Nótese que esta es la distancia al cuadrado. (!)
{-# INLINE distSqrdEuclid #-}
distSqrdEuclid :: Instance -> Instance -> Distance
distSqrdEuclid (x,_) (y,_) = sum $ map (** 2.0) (zipWith (-) x y)


-- Implementación del clasificador 1-KNN. Un clasificador toma un
-- problema y una instancia y nos devuelve si es de la clase correcta.
  -- Podemos medirlo directamente en booleanos o usando enteros que
-- representen a los booleanos.
knn :: Solution -> (Problem -> Instance -> Bool)
knn w ins a = uncurry (==) ((knn' ins &&& snd) a)
  where
    knn' :: Problem -> Instance -> Class
    knn' j b = snd $ minimumBy (comparing fst) $ map (distSqrd w b &&& snd) j

hit :: Bool -> Int
hit True = 1
hit False = 0

knnHit :: Solution -> (Problem -> Instance -> Int)
knnHit w i = hit . knn w i




-- Normalización de atributos. La normalización toma un conjunto de
-- instancias con sus atributos y transforma los atributos al intervalo
-- [0,1] linealmente.
normalizeDataset :: Problem -> Problem
normalizeDataset = normalize
  where
    normalize :: [Instance] -> [Instance]
    normalize instsclasses = zip (normalize' insts) classes
      where
        (insts,classes) = unzip instsclasses
        
    normalize' :: [[Value]] -> [[Value]]
    normalize' a = map (zipWith (\(m,d) i -> (i - m) / d) (zip minvector diffvector)) a
      where
        minvector :: [Value]
        minvector = foldr1 (zipWith min) a
        maxvector :: [Value]
        maxvector = foldr1 (zipWith max) a
        diffvector :: [Value]
        diffvector = zipWith (-) maxvector minvector
