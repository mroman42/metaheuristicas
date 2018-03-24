-- Fivefold parte los datasets en cinco trozos balanceados.
module Fivefold where

import Data.List
import Text.Printf
import System.Environment
import Base
import Input
  
showProblem :: Problem -> String
showProblem p = unlines $ map showInstance p
  where
    showInstance :: Instance -> String
    showInstance (l,c) = intercalate "," (map (printf "%f") l) ++ "," ++ show c

main :: IO ()
main = do
  args <- getArgs
  
  case args of
    [filename] -> do
      datasets <- fivesplit <$> readArff filename
      let datasetsName = zip (map (\n -> filename ++ ".part" ++ show n) ([1..] :: [Int])) (map showProblem datasets)
      mapM_ (uncurry writeFile) datasetsName
      
    _ -> putStrLn "El programa toma como argumento un archivo."


