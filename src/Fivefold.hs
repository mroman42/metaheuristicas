-- Fivefold parte los datasets en cinco trozos balanceados.
module Fivefold where

import Data.List
import Data.List.Split
import Text.Printf
import System.Environment

import Input

-- Definiciones previas
type Class = Double
type Value = Double
type Instance = ([Value],Class)
type Problem = [Instance]

-- Parte un problema en cinco trozos aproximadamente iguales
-- (diferirán a lo sumo en un elemento) y aproximadamente balanceados
-- de la misma forma que lo están en el conjunto original.
fivesplit :: Problem -> [Problem]
fivesplit a = zipWith (++) chunks1 chunks2
  where
    class1 = filter (\u -> snd u == 1.0) a
    class2 = filter (\u -> snd u == 2.0) a
    clen1 = quot (length class1) 5 + if rem (length class1) 5 == 0 then 0 else 1
    clen2 = quot (length class2) 5 + if rem (length class2) 5 == 0 then 0 else 1    
    chunks1 = chunksOf clen1 class1
    chunks2 = chunksOf clen2 class2

-- Devuelve una lista de instancias como una cadena para imprimirla en
-- un archivo.
showProblem :: Problem -> String
showProblem p = unlines $ map showInstance p
  where
    showInstance :: Instance -> String
    showInstance (l,c) = intercalate "," (map (printf "%f") l) ++ "," ++ show c

-- El programa toma como único argumento un archivo de datos en
-- formato arff, que partirá en cinco archivos a los que les añade un
-- .partN, siendo N el número de parte.
main :: IO ()
main = do
  args <- getArgs
  
  case args of 
    [filename] -> do
      datasets <- fivesplit <$> readArff filename
      let datasetsName =
             zip (map (\n -> filename ++ ".part" ++ show n) ([1..] :: [Int])) (map showProblem datasets)
      mapM_ (uncurry writeFile) datasetsName
      
    _ -> putStrLn "El programa toma como argumento un archivo."


