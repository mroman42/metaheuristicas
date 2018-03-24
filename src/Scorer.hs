module Scorer where

import Control.Monad
import System.IO
import System.Environment
import Base
import Input




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
