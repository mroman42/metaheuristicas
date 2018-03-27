{-# LANGUAGE BangPatterns #-}
module LocalSearch where

import Data.Random.Normal
import System.Random
import System.Random.Shuffle
import System.Environment
import Control.DeepSeq
import Data.Time
--import Data.Sequence as S

import Input
import Base (nAttr)
import LeaveOneOut

type Class = Double
type Value = Double
type Weight = Double
type UnWeight = Double
type Solution = [Weight] --S.Seq Weight
type Instance = ([Value],Class)
type Problem = [Instance]
type Seed = Int


-- Determina la desviación estándar de las distribuciones normales.
{-# INLINE sigma #-}
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
  , fitness :: !Double
  }

-- Da un paso de exploración, que puede ser exitoso y llevar a una
-- nueva solución w' con mejor resultado en la función objetivo, o no
-- serlo y quedarse en el mismo punto. En ambos casos, llevará unos
-- contadores
explore :: Problem -> Environment -> Environment

explore training (Environment gStp lStp g [] w f) =
  explore training (Environment gStp lStp g (shuffle' [0..length w-1] (length w) g) w f)
  
explore training (Environment gStp lStp g (i:p) w f)
  | f < f'    = Environment (gStp+1) 0        g' [] w' f'
  | otherwise = Environment (gStp+1) (lStp+1) g' p  w  f
  where
    w' = mutation epsilon i w :: Solution
    f' = objective training w' :: Double
    (epsilon, g') = normal' (0,sigma) g 

 
-- Explora hasta alcanzar cualquiera de las dos cotas.
localSearch :: Seed -> Problem -> Solution
localSearch s !training = solution $ until
  (\env -> globalSteps env == 15000 || localSteps env == 20 * nAttr training)
  (explore training)
  initialEnvironment
  where
    initialEnvironment :: Environment
    initialEnvironment = Environment 0 0 (mkStdGen s) [] initialSolution (objective training initialSolution)
    initialSolution :: Solution
    initialSolution = take (nAttr training) $ randomRs (0.5,0.5) (mkStdGen s)


templateMain :: (Seed -> Problem -> Solution) -> IO ()
templateMain alg = do
  -- Input
  seed <- read . Prelude.head <$> getArgs
  training <- string2ProblemNorm <$> getContents

  -- Timing
  start <- getCurrentTime
  let sltn = alg seed training
  stop <- sltn `deepseq` getCurrentTime
  let diffTime = diffUTCTime stop start

  -- Output
  putStrLn $ "@time: " Prelude.++ show diffTime Prelude.++ "\n" Prelude.++ showSolution sltn



main :: IO ()
main = templateMain localSearch
