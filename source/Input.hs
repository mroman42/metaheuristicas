module Input where

import Text.Read
import System.IO
import Data.List
import Data.List.Split
import Control.Arrow
import Base


-- Nombres de archivo que usará como entrada.
type Filename = String
fileOzone, fileParkinson, fileHeart :: Filename
fileOzone = "./apc/ozone-320.arff"
fileParkinson = "./apc/parkinsons.arff"
fileHeart = "./apc/spectf-heart.arff"


-- Lee un problema desde un archivo.
readArff :: Filename -> IO Problem
readArff filename = do
  handle <- openFile filename ReadMode
  contents <- tail . tail . lines <$> hGetContents handle
  -- let secAttrClass = takeWhile (isPrefixOf "@attribute") contents
  -- let secAttr = init secAttrClass
  let secData = dropWhile (\ s -> isPrefixOf "@" s || s == "") contents
  -- let attributes = map (reverse . drop (length " numeric") . reverse . drop (length "@attribute")) secAttr
  let rawdata = map (map (check . (id &&& readMaybe)) . splitOn ",") secData :: [[Value]]
  let values = map (init &&& last) rawdata
  return values
    where
      check :: (String , Maybe Double) -> Double
      check (_,Just f) = f
      check (x,Nothing) = error ("Error reading ->" ++ x ++ "<-")
