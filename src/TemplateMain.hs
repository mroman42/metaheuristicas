module TemplateMain where

import System.Environment
import Control.DeepSeq
import Data.Time
import Base
import Input

-- Standard version
templateMain :: (Seed -> Problem -> Solution) -> IO ()
templateMain alg = do
  -- Input
  seed <- read . Prelude.head <$> getArgs
  training <- string2ProblemNorm <$> getContents

  -- Timing
  start <- getCurrentTime
  let solution = alg seed training
  stop <- solution `deepseq` getCurrentTime
  let diffTime = diffUTCTime stop start

  -- Output
  putStrLn $ "@time: " Prelude.++ show diffTime Prelude.++ "\n" Prelude.++ showSolution solution



-- Vector version
-- type Point = UV.Vector Double
-- type VSolution = UV.Vector Double
-- data VInstance = VInstance {label :: !Int, point :: !Point}
-- type VProblem = V.Vector VInstance

-- vTemplateMain :: (Seed -> VProblem -> VSolution) -> IO ()
-- vTemplateMain f = templateMain (\s p -> UV.toList $ f s (fromProblem p))
--   where
--     fromProblem :: Problem -> VProblem
--     fromProblem prob = V.fromList $ Prelude.map fromInstance prob
--     fromInstance :: Instance -> VInstance
--     fromInstance (q,cls) = VInstance (round cls) (UV.fromList q)
