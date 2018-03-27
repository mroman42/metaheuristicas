{-# LANGUAGE BangPatterns #-}
module LocalSearch where

import Data.Random.Normal
import System.Random
import System.Random.Shuffle

import TemplateMain
import Base (nAttr)
import LeaveOneOut

type Class = Double
type Value = Double
type Weight = Double
type UnWeight = Double
type Solution = [Weight]
type Instance = ([Value],Class)
type Problem = [Instance]
type Seed = Int


-- Determina la desviación estándar de las distribuciones normales.
sigma :: Double
sigma = 0.3


-- Aplica una mutación epsilon en el índice dado.
mutation :: Double -> Int -> Solution -> Solution
mutation epsilon indx w = map (\(x,i) -> if i /= indx then x else trunc $ x + epsilon) (zip w [1..])
  where
    trunc :: UnWeight -> Weight
    trunc x
      | x > 1 = 1
      | x < 0 = 0 
      | otherwise = x


-- Estructura con los datos de la búsqueda local. Lleva un contador
-- global, un contador local, un generador aleatorio para la
-- distribución normal y una permutación de la que ir extrayendo
-- índices para aplicar mutaciones. Esta estructura se irá
-- actualizando conforme avance la búsqueda.
data Environment = Environment 
  { globalSteps :: Int
  , localSteps :: Int
  , gen :: StdGen
  , permutation :: [Int]  
  , solution :: Solution
  }

-- Da un paso de exploración, que puede ser exitoso y llevar a una
-- nueva solución w' con mejor resultado en la función objetivo, o no
-- serlo y quedarse en el mismo punto. En ambos casos, llevará unos
-- contadores
explore :: Problem -> Environment -> Environment

explore training (Environment gStp lStp g [] w) =
  explore training (Environment gStp lStp g (shuffle' [0..length w-1] (length w) g) w)
  
explore training (Environment gStp lStp g (i:p) w)
  | objective training w < objective training w' = Environment (gStp+1) 0        g' [] w'
  | otherwise                                    = Environment (gStp+1) (lStp+1) g' p  w
  where
    w' = mutation epsilon i w :: Solution
    (epsilon, g') = normal' (0,sigma) g 

 
-- Explora hasta alcanzar cualquiera de las dos cotas.
localSearch' :: Seed -> Problem -> Solution
localSearch' s training = solution $ until
  (\env -> globalSteps env == 15000 || localSteps env == 20 * nAttr training)
  (explore training)
  initialEnvironment
  where
    initialEnvironment :: Environment
    initialEnvironment = Environment 0 0 (mkStdGen s) [] initialSolution
    initialSolution :: Solution
    initialSolution = take (nAttr training) $ randomRs (0.5,0.5) (mkStdGen s)



main :: IO ()
main = templateMain localSearch'

-- Devuelve una mejora local después de buscarla durante un tiempo.
-- localsearchstep :: (RandomGen g) => Int -> g -> Problem -> Solution -> Maybe Solution
-- localsearchstep searchBound gen training w =
--   -- Busca una mejora.
--   find (\w' -> objective training w < objective training w')
--   -- Limitada entre 20*n primeras variaciones que encuentre o entre la
--   -- cota global.
--   $ take (min searchBound (20 * length w))
--   -- Mutando a cada paso.
--   $ map (\(d,i) -> mutation d i w)
--   -- Según una distribución normal aplicada en cada paso a una
--   -- característica, elegida por una permutación de características.
--   $ zip
--     (normals' (0,sigma) gen)
--     (shuffle' (map (`mod` length w) [0..(20 * length w - 1)]) (20 * length w) gen)
    

-- localsearch :: (RandomGen g) => g -> Problem -> Solution -> Solution
-- localsearch gen training = acc gen 15000
--   where
--     acc :: (RandomGen g) => g -> Int -> Solution -> Solution
--     acc _    0 s = s
--     acc gen' n s = case localsearchstep n gen' training s of
--                      Just s' -> acc ((snd . next) gen') (n-1) s'
--                      Nothing -> s

-- localSearch :: Seed -> Problem -> Solution
-- localSearch seed p = localsearch (mkStdGen seed) p initialSolution
--   where
--     -- La solución inicial se obtiene de una distribución uniforme.
--     initialSolution :: Solution
--     initialSolution = take (nAttr p) $ randomRs (0.5,0.5) (mkStdGen seed)
