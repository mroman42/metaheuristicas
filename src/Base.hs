module Base where

import Data.List
import Data.List.Split
import Data.Ord
import Control.Arrow hiding (left, right)
import Text.Printf

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
oneOut :: ([a] -> a -> b) -> [a] -> [b]
oneOut f list = map (uncurry f) (acc [] list)
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
    clen1 = quot (length class1) 5 + if rem (length class1) 5 == 0 then 0 else 1
    clen2 = quot (length class2) 5 + if rem (length class2) 5 == 0 then 0 else 1    
    chunks1 = chunksOf clen1 class1
    chunks2 = chunksOf clen2 class2

-- Devuelve el problema en parejas de Training y Test.
fiveassignment :: Problem -> [(Training, Test)]
fiveassignment a = oneOut (\x y -> (concat x,y)) (fivesplit a)


data Report = Report
  { tasaClas :: Double
  , tasaRed :: Double
  , aggregate :: Double
  , time :: Double
  }
  deriving (Eq,Show)

mean :: [Double] -> Double
mean xs = sum xs / genericLength xs

aggregateReport :: [Report] -> Report
aggregateReport reports = Report
  (mean $ map tasaClas reports)
  (mean $ map tasaRed reports)
  (mean $ map aggregate reports)
  (sum  $ map time reports)


-- Devuelve la precisión y simplicidad de una solución dada.
report :: Solution -> Training -> Test -> Time -> Report
report w training test = Report
  (precision w training test)
  (simplicity w)
  (score w training test)


-- Puntuación media de las cinco ejecuciones en el conjunto de datos,
-- utilizando validación cruzada con cinco particiones.
fivefold :: (Problem -> Solution) -> Problem -> Double
fivefold heuristic dataset = (\(x,_,_)->x) $ acc (0,[],datasplit)
  where
    datasplit = fivesplit dataset

    acc :: (Double,[Problem],[Problem]) -> (Double,[Problem],[Problem])
    acc (f,_,[]) = (f,[],[])
    acc (f,l,s:r) = (f + score (heuristic (concat (l++r))) (concat (l++r)) s, s:l, r)

-- Muestra un reporte agregado de la combinación de los reportes obtenidos
-- haciendo validación cruzada entre 5 particiones.
-- reportFiveFold :: (Problem -> Solution) -> Problem -> [Report]
-- reportFiveFold h dataset = reports ++ [aggregateReport reports]
--   where
--     reports = map (\(training , test) -> report (h training) training test) (fiveassignment dataset)





-- Dibuja las tablas de salida del programa
drawReport :: Report -> String
drawReport r = intercalate "," $ map (printf "%.5f" . ($ r)) [tasaClas, tasaRed, aggregate, time]

drawReports :: [Report] -> String
drawReports l = unlines $ map drawReport l

-- drawCompleteReport :: (Problem -> Solution) -> Problem -> String
-- drawCompleteReport h a = drawReports $ reportFiveFold h a

-- printReport :: (Problem -> Solution) -> Problem -> IO ()
-- printReport h p = putStr $ drawCompleteReport h p

