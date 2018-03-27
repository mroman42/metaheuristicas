{-# LANGUAGE BangPatterns #-}
module LocalSearch where

import Data.Random.Normal
import System.Random
import System.Random.Shuffle
import System.Environment
import Control.DeepSeq
import Control.Monad.Loops
import Data.Time
import Data.Vector.Unboxed.Mutable as MUV
import Data.Vector.Unboxed as UV

import Input
import Base (nAttr)
import LeaveOneOut

type Class = Double
type Value = Double
type Weight = Double
type UnWeight = Double
type Solution = MUV.IOVector Double
type Instance = ([Value],Class)
type Problem = [Instance]
type Seed = Int


-- Determina la desviación estándar de las distribuciones normales.
{-# INLINE sigma #-}
sigma :: Double
sigma = 0.3


-- Aplica una mutación epsilon en el índice dado.
mutation :: Double -> Int -> Solution -> IO Double
mutation epsilon indx w = do
  prev <- MUV.read w indx
  MUV.modify w (trunc . (+ epsilon)) indx
  return prev
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
explore :: Problem -> Environment -> IO Environment

explore training (Environment gStp lStp g [] w f) =
  explore training (Environment gStp lStp g (shuffle' [0..MUV.length w-1] (MUV.length w) g) w f)
  
explore training (Environment gStp lStp g (i:p) w f) = do
  -- Realiza una mutación y calcula su bondad
  prev <- mutation epsilon i w
  freezedw <- UV.unsafeFreeze w
  let f' = objective' training freezedw
  w' <- UV.unsafeThaw freezedw

  -- Si es mejor que la anterior, sigue con ella, si no, deshace la
  -- mutación.
  if f < f'
    then
      return $ Environment (gStp+1) 0 g' [] w' f'
    else do
      write w' i prev
      return $ Environment (gStp+1) (lStp+1) g' p w' f
  where
    (epsilon, g') = normal' (0,sigma) g 

 
-- Explora hasta alcanzar cualquiera de las dos cotas.
localSearch :: Seed -> Problem -> IO Solution
localSearch s !training = do
  initEnv <- initialEnvironment
  solution <$>
    iterateUntilM (\env -> globalSteps env == 15000 || localSteps env == 20 * nAttr training)
    (explore training)
    initEnv
  where
    initialEnvironment :: IO Environment
    initialEnvironment = do
      initSol <- initialSolution
      freezedSol <- UV.unsafeFreeze initSol
      let o = objective' training freezedSol
      initSol <- UV.unsafeThaw freezedSol
      return $ Environment 0 0 (mkStdGen s) [] initSol o
    initialSolution :: IO Solution
    initialSolution = UV.thaw $ UV.take (nAttr training) $ UV.fromList $ randomRs (0.5,0.5) (mkStdGen s)




main :: IO ()
main = do
  -- Input
  seed <- Prelude.read . Prelude.head <$> getArgs
  training <- string2ProblemNorm <$> getContents

  -- Timing
  start <- getCurrentTime
  sltn <- localSearch seed training
  stop <- sltn `deepseq` getCurrentTime
  let diffTime = diffUTCTime stop start

  freezedSltn <- UV.freeze sltn
  
  -- Output
  putStrLn $ "@time: " Prelude.++ show diffTime Prelude.++ "\n" Prelude.++ showSolution (UV.toList freezedSltn)

