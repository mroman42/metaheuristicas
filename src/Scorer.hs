module Scorer where

import Control.Monad
import Text.Printf
import System.IO
import System.Environment
import Data.List

import Base
import Input


-- Mide la precisión de una solución bajo un conjunto de Training y un
-- conjunto de Test. Implementa la tasa_clas que se define en el guion
-- de la práctica.
precision :: Solution -> Problem -> Problem -> Double
precision w training test = fromIntegral hits / fromIntegral (length test)
  where
    hits :: Int
    hits = sum $ map (knnHit w' training) test
    w' = map (\x -> if x < 0.2 then 0 else x) w


-- Mide la simplicidad de una solución, implementa la "tasa_red" definida
-- en el guion de la práctica. Para ello comprueba cuantos pesos quedan por
-- debajo de 0.2.
simplicity :: Solution -> Double
simplicity w = fromIntegral (length (filter (< 0.2) w)) / fromIntegral (length w)

-- Mide la puntuación que obtiene una clasificación, combinando su precisión
-- y su simplicidad.
score :: Solution -> Problem -> Problem -> Double
score w training test = alpha * precision w' training test + (1 - alpha) * simplicity w
  where
    -- El valor de alpha puede cambiarse aquí.
    alpha = 0.5
    -- Truncando los pesos
    w' = map (\x -> if x < 0.2 then 0 else x) w


-- Report completo de la bondad de una solución.
data Report = Report
  { tasaClas :: Double
  , tasaRed :: Double
  , aggregate :: Double
  , time :: Double
  }
  deriving (Eq,Show)

-- Devuelve la precisión y simplicidad de una solución dada.
report :: Solution -> Training -> Test -> Time -> Report
report w training test = Report
  (precision w training test)
  (simplicity w)
  (score w training test)


showReport :: Report -> String
showReport r = intercalate "," $ map (printf "%.3f") [tasaClas r, tasaRed r, aggregate r, time r]

main :: IO ()
main = do
  -- Recibe en la entrada estándar el conjunto de training; toma como
  -- argumentos la solución (que son los pesos de un knn sobre training), y
  -- el conjunto de test.
  args <- getArgs
  let filesolution = head args
  let filetest = args !! 1

  (stime,solution) <- readSolutionFile <$> join (hGetContents <$> openFile filesolution ReadMode)
  test <- string2ProblemNorm <$> join (hGetContents <$> openFile filetest ReadMode)
  training <- string2ProblemNorm <$> getContents

  let rp = report solution training test stime
  putStrLn (showReport rp)
