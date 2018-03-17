module Algorithms where

import Prelude hiding (zipWith, map)
import Data.Array.Accelerate

type Class = Float
type Value = Float

type AccInstance = Acc (Vector Value)
type AccSubset = Vector (Vector Value , Class)
type Solution = Acc (Vector Float)

knn :: Array DIM2 Float -> Acc (Vector Class) -> AccInstance -> Class
knn instances classes inst = error "notyet"

dist :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dist a b = fold (+) 0 $ map (** 2.0) (zipWith (-) a b)
