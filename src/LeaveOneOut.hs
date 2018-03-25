module LeaveOneOut where

-- import Control.Arrow
import Data.Ord
import Data.List
-- import Data.Array.Accelerate


import Base (Problem, Solution, Instance, Distance, Value, Weight)

-- TODO: LeaveOneOut
objective :: Problem -> Solution -> Double
objective training w = fromIntegral (knn' w training) / fromIntegral (length training)

hit :: Bool -> Int
hit True = 1
hit False = 0

dist :: Solution -> Instance -> Instance -> Distance
dist v x y = dist' v (fst x) (fst y)
  where
    dist' :: [Weight] -> [Value] -> [Value] -> Distance
    dist' w a b = sum $ zipWith (*) w' $ map (** 2.0) (zipWith (-) a b)
      where
        w' :: [Weight]
        w' = map (\z -> if z < 0.2 then 0 else z) w

knn' :: Solution -> Problem -> Int
knn' w p = sum
  $ map (hit . snd . minimumBy (comparing fst)
    . \(j,a) -> map (\(k,b) -> if j == k then (infinity,snd a == snd b) else (dist w a b,snd a == snd b)) p') p'
  where
    p' :: [(Int, Instance)]
    p' = zip ([1..] :: [Int]) p
    
    infinity :: Double
    infinity = 1.0/0.0


-- oneOut :: ([a] -> a -> b) -> [a] -> [b]
-- oneOut f list = map (uncurry f) (acc [] list)
--   where
--     acc :: [a] -> [a] -> [([a],a)]
--     acc _ [] = []
--     acc l (a:r) = (l ++ r, a) : acc (a:l) r    

-- knn :: Solution -> (Problem -> Instance -> Bool)
-- knn w ins a = uncurry (==) ((knn' ins &&& snd) a)
--   where
--     knn' :: Problem -> Instance -> Class
--     knn' j b = snd $ minimumBy (comparing fst) $ map (dist w b &&& snd) j

-- Función objetivo de un problema. Devuelve el agregado de precisión
-- y simplicidad de la solución, habiendo comprobado la precisión
-- sobre su propio conjunto de entrenamiento dejando a uno de los
-- elementos del conjunto fuera.
-- objective :: Problem -> Solution -> Double
-- objective p w = _

-- knn :: Solution -> (Problem -> Instance -> Bool)
-- knn w ins a = uncurry (==) ((knn' ins &&& snd) a)
--   where
--     knn' :: Problem -> Instance -> Class
--     knn' j b = snd $ minimumBy (comparing fst) $ map (dist w b &&& snd) j


-- knnHit :: Solution -> (Problem -> Instance -> Int)
-- knnHit w i = hit . knn w i
