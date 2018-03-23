module Base where

import Data.List
import Data.List.Split
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



-- Leave one out. Esta técnica toma una lista y aplica una función de
-- argumentos "[a]" y "a" a cada elemento de la lista contra todo el
-- resto de la lista.
oneOut :: [a] -> ([a] -> a -> b) -> [b]
oneOut list f = map (uncurry f) (acc list [])
  where
    acc :: [a] -> [a] -> [([a],a)]
    acc _ [] = []
    acc l (a:r) = (l ++ r, a) : acc (a:l) r





-- Mide la precisión de una solución bajo un conjunto de Training y un
-- conjunto de Test. Implementa la tasa_clas que se define en el guion
-- de la práctica.
precision :: Solution -> Problem -> Problem -> Double
precision w training test = fromIntegral hits / fromIntegral (length test)
  where
    hits :: Int
    hits = sum $ map (knnHit w training) test


-- Mide la simplicidad de una solución, implementa la "tasa_red" definida
-- en el guion de la práctica. Para ello comprueba cuantos pesos quedan por
-- debajo de 0.2.
simplicity :: Solution -> Double
simplicity w = fromIntegral (length (filter (< 0.2) w)) / fromIntegral (length w)

-- Mide la puntuación que obtiene una clasificación, combinando su precisión
-- y su simplicidad.
score :: Solution -> Problem -> Problem -> Double
score w training test = alpha * precision w training test + (1 - alpha) * simplicity w
  where
    alpha :: Double
    alpha = 0.5




-- Parte un problema en cinco trozos aproximadamente iguales
-- (diferirán a lo sumo en un elemento) y aproximadamente balanceados
-- de la misma forma que lo están en el conjunto original.
fivesplit :: Problem -> [Problem]
fivesplit a = zipWith (++) chunks1 chunks2
  where
    class1 = filter (\u -> snd u == 1.0) a
    class2 = filter (\u -> snd u == 2.0) a
    clen = quot (length a) 5 + if rem (length a) 5 == 0 then 0 else 1
    chunks1 = chunksOf clen class1
    chunks2 = chunksOf clen class2

-- Puntuación media de las cinco ejecuciones en el conjunto de datos,
-- utilizando validación cruzada con cinco particiones.
fivefold :: (Problem -> Solution) -> Problem -> Double
fivefold heuristic dataset = (\(x,_,_)->x) $ acc (0,[],datasplit)
  where
    datasplit = fivesplit dataset

    acc :: (Double,[Problem],[Problem]) -> (Double,[Problem],[Problem])
    acc (f,_,[]) = (f,[],[])
    acc (f,l,s:r) = (f + score (heuristic (concat (l++r))) (concat (l++r)) s, s:l, r)
