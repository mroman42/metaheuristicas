module LocalSearch where

import Data.List
import Data.Random.Normal
import System.Random
import System.Random.Shuffle

-- import Data.Sequence

import TemplateMain
import Base
import LeaveOneOut

-- Determina la desviación estándar de las distribuciones normales.
type Sigma = Double
sigma :: Sigma
sigma = 0.3



-- Trunca los pesos al intervalo [0,1].
truncateWeights :: [UnWeight] -> [Weight]
truncateWeights = map (\x -> if x > 1 then 1 else if x < 0 then 0 else x)



-- Aplica una mutación epsilon en el índice dado.
mutation :: Double -> Int -> Solution -> Solution
mutation epsilon indx w = map (\(x,i) -> if i /= indx then x else x + epsilon) (zip w [1..])

-- Devuelve una mejora local después de buscarla durante un tiempo.
localsearchstep :: (RandomGen g) => Int -> g -> Problem -> Solution -> Maybe Solution
localsearchstep searchBound gen training w =
  -- Busca una mejora.
  find (\w' -> objective training w < objective training w')
  -- Limitada entre 20*n primeras variaciones que encuentre o entre la
  -- cota global.
  $ take (min searchBound (20 * length w))
  -- Mutando a cada paso.
  $ map (\(d,i) -> mutation d i w)
  -- Según una distribución normal aplicada en cada paso a una
  -- característica, elegida por una permutación de características.
  $ zip
    (normals' (0,sigma) gen)
    (shuffle' (map (`mod` length w) [1..(20 * length w)]) (20 * length w) gen)

localsearch :: (RandomGen g) => g -> Problem -> Solution -> Solution
localsearch gen training = acc gen 1500
  where
    acc :: (RandomGen g) => g -> Int -> Solution -> Solution
    acc _    0 s = s
    acc gen' n s = case localsearchstep n gen' training s of
                     Just s' -> acc ((snd . next) gen') (n-1) s'
                     Nothing -> s

localSearch :: Seed -> Problem -> Solution
localSearch seed p = localsearch (mkStdGen seed) p initialSolution
  where
    -- La solución inicial se obtiene de una distribución uniforme.
    initialSolution :: Solution
    initialSolution = take (nAttr p) $ randomRs (0.0,1.0) (mkStdGen seed)


main :: IO ()
main = templateMain localSearch
