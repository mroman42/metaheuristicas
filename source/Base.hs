module Base where

import Data.List
import Data.Ord
import Control.Arrow

-- Las instancias se clasifican en varias clases. Cada una de ellas
-- está representada por su vector de valores para cada atributo. Un
-- problema está dado por un conjunto de instancias clasificadas.
type Class = Double
type Value = Double
type Instance = ([Value],Class)
type Problem = [Instance]

-- Propiedades de los problemas.
-- | Número de atributos que tiene un problema.
nAttr :: Problem -> Int
nAttr [] = 0
nAttr (a:_) = length a




-- Los pesos vienen dados por flotantes. Una solución es un vector
-- de pesos. Los pesos deben estar en el intervalo [0,1], mientras
-- que consideramos pesos no normalizados con otro tipo distinto.
type Weight = Double
type UnWeight = Double
type Solution = [Weight]

-- Una heurística toma un problema y devuelve una solución.
type Heuristic = Problem -> Solution



-- Las distancias vienen dadas en flotantes.
type Distance = Double


-- Mide la distancia euclídea con pesos entre dos instancias.
-- Los pesos menores que 0.2 se descartan automáticamente.
dist :: Solution -> Instance -> Instance -> Distance
dist v x y = dist' v (fst x) (fst y)
  where
    dist' :: [Weight] -> [Value] -> [Value] -> Distance
    dist' w a b = sum $ zipWith (*) w' $ map (** 2.0) (zipWith (-) a b)
      where
        w' :: [Weight]
        w' = map (\z -> if z < 0.2 then 0 else z) w    




-- Implementación del clasificador 1-KNN. Un clasificador toma un
-- problema y una instancia y nos devuelve si es de la clase correcta.
-- Podemos medirlo directamente en booleanos o usando enteros que
-- representen a los booleanos.
knn :: Solution -> (Problem -> Instance -> Bool)
knn w ins a = uncurry (==) ((knn' w ins &&& snd) a)
  where
    knn' :: Solution -> Problem -> Instance -> Class
    knn' v j b = snd $ minimumBy (comparing fst) $ map (dist v b &&& snd) j

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
