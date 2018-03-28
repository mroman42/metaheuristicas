{-# LANGUAGE BangPatterns #-}
module LocalSearch2 where

import Data.Random.Normal
import System.Random
import System.Random.Shuffle
import System.Environment
import Control.DeepSeq
import Data.Time

import Input
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
{-# INLINE sigma #-}
sigma :: Double
sigma = 0.2


-- Aplica una mutación epsilon en un vector completo.
mutation :: StdGen -> Solution -> Solution
mutation g w = map trunc $ zipWith (+) w $ normals' (0,sigma) g
  where
    trunc :: UnWeight -> Weight  
    trunc x
      | x > 1 = 1 
      | x < 0.2 = 0 
      | otherwise = x 

-- Estructura con los datos de la búsqueda local. Lleva un contador
-- global, un contador local, un generador aleatorio para la
-- distribución normal y una permutación de la que ir extrayendo
-- índices para aplicar mutaciones. Esta estructura se irá
-- actualizando conforme avance la búsqueda.
data Environment = Environment 
  { globalSteps :: !Int
  , localSteps :: !Int
  , gen :: StdGen
  , permutation :: ![Int]  
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
  
explore training (Environment gStp lStp g (_:p) w f)
  | f < f'    = Environment (gStp+1) 0        g' p' w' f'
  | otherwise = Environment (gStp+1) (lStp+1) g' p  w  f
  where
    w' = mutation g w :: Solution
    f' = objective training w' :: Double
    p' = shuffle' [0..length w-1] (length w) g'
    (_, g') = normal' (0,sigma) g

 
-- Explora hasta alcanzar cualquiera de las dos cotas desde una
-- solución determinada. 
{-# INLINE localSearchFrom #-}
localSearchFrom :: Seed -> Problem -> Solution -> Solution
localSearchFrom s !training !initial = solution $ until
  (\env -> globalSteps env == 15000 || localSteps env == 20 * nAttr training)
  (explore training)
  initialEnvironment
  where
    initialEnvironment :: Environment
    initialEnvironment = Environment 0 0 (mkStdGen s) [] initial (objective training initial)


-- Búsqueda local desde una solución aleatoria.
localSearch :: Seed -> Problem -> Solution
localSearch s training = localSearchFrom s training initialSolution
  where
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
