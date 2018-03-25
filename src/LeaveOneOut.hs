{-# LANGUAGE BangPatterns #-}
module LeaveOneOut where


import Base (Problem, Solution, Instance)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Strategies



objective :: Problem -> Solution -> Double
objective = objectiveVector


-- http://lpaste.net/105456 
objectiveVector :: Problem -> Solution -> Double 
objectiveVector p s = aggregate (fromProblem p) (fromSolution s)
  where
    fromProblem :: Problem -> V.Vector LabelPoint
    fromProblem prob = V.fromList $ map fromInstance prob
    fromInstance :: Instance -> LabelPoint
    fromInstance (q,cls) = LabelPoint (round cls) (UV.fromList q)
    fromSolution :: Solution -> UV.Vector Double
    fromSolution = UV.fromList

type Point = UV.Vector Double
data LabelPoint = LabelPoint {label :: !Int, point :: !Point}

-- TODO: Truncar los pesos
classify :: V.Vector LabelPoint -> UV.Vector Double -> Point -> Int
classify !training !weights !points = label mini
  where
    distsqrd = UV.sum . UV.zipWith (****) weights . UV.map (^2) . UV.zipWith (-) points
    mini = training V.! V.minIndex (V.map (discardZeroes . distsqrd . point) training)
    -- Leave one out
    discardZeroes z = if z == 0 then 1/0 else z
    -- Multiplicación truncada de los pesos
    (****) a b = if a < 0.2 then 0 else a * b
    
validate :: V.Vector LabelPoint -> UV.Vector Double -> Double
validate !trainingSet !weights = correct
  where
    isCorrect (LabelPoint l p) = fromEnum $ classify trainingSet weights p == l
    numCorrect = V.sum (V.map isCorrect trainingSet `using` parVector 1)
    correct = fromIntegral numCorrect / fromIntegral (V.length trainingSet) 

simplicity :: UV.Vector Double -> Double
simplicity !weights = UV.sum $ UV.map (\w -> if w < 0.2 then 1 else 0) weights

aggregate :: V.Vector LabelPoint -> UV.Vector Double -> Double
aggregate !trainingSet !weights = validate trainingSet weights + simplicity weights
