module Input where

import Text.Read
import Text.Printf
import System.IO
import Data.List
import Data.List.Split
import Control.Arrow
import Base


-- Nombres de archivo que usará como entrada.
type Filename = String
fileOzone, fileParkinson, fileHeart :: Filename
fileOzone = "./bin/ozone-320.arff"
fileParkinson = "./bin/parkinsons.arff"
fileHeart = "./bin/spectf-heart.arff"


-- Lee un problema desde un archivo.
readArff :: Filename -> IO Problem
readArff filename = do
  handle <- openFile filename ReadMode
  contents <- tail . tail . lines <$> hGetContents handle
  let secData = dropWhile (\ s -> isPrefixOf "@" s || s == "") contents
  let rawdata = map (map (check . (id &&& readMaybe)) . splitOn ",") secData :: [[Value]]
  let values = map (init &&& last) rawdata
  return values
    where
      check :: (String , Maybe Double) -> Double
      check (_,Just f) = f
      check (x,Nothing) = error ("Error reading ->" ++ x ++ "<-")

readPartNormalized :: Filename -> IO Problem
readPartNormalized filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return (string2ProblemNorm contents)

string2Problem :: String -> Problem
string2Problem contents = map (init &&& last) (map (map read . splitOn ",") $ lines contents :: [[Value]])

string2ProblemNorm :: String -> Problem
string2ProblemNorm = normalizeDataset . string2Problem

showSolution :: Solution -> String
showSolution s = intercalate "," (map (printf "%f") s)

                     
