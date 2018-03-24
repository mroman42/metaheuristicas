module Onenn where

import System.Environment
import Control.DeepSeq
import Data.Time
import Base
import Input

onenn :: Problem -> Solution
onenn p = replicate (nAttr p) 1

main :: IO ()
main = do
  -- Input
  args <- getArgs
  training <- string2ProblemNorm <$> getContents

  -- Timing
  start <- getCurrentTime
  let solution = onenn training
  stop <- solution `deepseq` getCurrentTime
  let diffTime = diffUTCTime stop start

  -- Output
  putStrLn $ "@time: " ++ show diffTime ++ "\n" ++ showSolution (onenn training)
