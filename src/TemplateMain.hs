module TemplateMain where

import System.Environment
import Control.DeepSeq
import Data.Time
import Base
import Input

templateMain :: (Seed -> Problem -> Solution) -> IO ()
templateMain alg = do
  -- Input
  seed <- read . head <$> getArgs
  training <- string2ProblemNorm <$> getContents

  -- Timing
  start <- getCurrentTime
  let solution = alg seed training
  stop <- solution `deepseq` getCurrentTime
  let diffTime = diffUTCTime stop start

  -- Output
  putStrLn $ "@time: " ++ show diffTime ++ "\n" ++ showSolution solution
