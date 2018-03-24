import Data.List
import Data.Ord
import Data.Random.Normal
import System.Random
import Options hiding (defaultOptions)


import Base
import Input


-- knn as an algorithm
knnTrivial :: Problem -> Solution
knnTrivial a = replicate (nAttr a) 1



-- Greedy: RELIEF
ofClass :: Bool -> Class -> [Instance] -> [Instance]
ofClass True c ins = filter (\x-> snd x == c) ins
ofClass False c ins = filter (\x-> snd x /= c) ins

friendsOf :: Instance -> [Instance] -> [Instance]
friendsOf a = ofClass True (snd a)
enemiesOf :: Instance -> [Instance] -> [Instance]
enemiesOf a = ofClass False (snd a)

closestTo :: [Weight] -> Instance -> [Instance] -> Instance
closestTo w a = minimumBy (comparing (dist w a))
closestFriend :: [Weight] -> Instance -> [Instance] -> Instance
closestFriend w a ins = closestTo w a $ friendsOf a ins
closestEnemy :: [Weight] -> Instance -> [Instance] -> Instance
closestEnemy w a ins = closestTo w a $ enemiesOf a ins

normalizeWeights :: [UnWeight] -> [Weight]
normalizeWeights w = map ((/ maximum w) . negtrunc) w
  where
    negtrunc :: UnWeight -> UnWeight
    negtrunc a = if a < 0 then 0 else a

reliefUpdateWith :: [Weight] -> [Instance] -> Instance -> [Weight]
reliefUpdateWith w ins a = normalizeWeights $ zipWith (+) (zipWith (-) w dimb) dimc
  where
    b = closestFriend w a ins
    c = closestEnemy w a ins
    dimb = map abs (zipWith (-) (fst a) (fst b))
    dimc = map abs (zipWith (-) (fst a) (fst c))

relief' :: [Weight] -> [Instance] -> [Weight]
relief' w ins = (\(v,_,_) -> v) $ acc (w,[],ins)
  where
    acc :: ([Weight],[Instance],[Instance])
        -> ([Weight],[Instance],[Instance])
    acc (v,_,[]) = (v,[],[])
    acc (v,l,a:r) = acc (reliefUpdateWith v (l ++ r) a, a:l , r)

relief :: Problem -> Solution
relief = relief' [0,0..]









-- LOCAL SEARCH
truncateWeights :: [UnWeight] -> [Weight]
truncateWeights = map (\x -> if x > 1 then 1 else if x < 0 then 0 else x)

mutation :: Double -> Int -> Solution -> Solution
mutation sigma seed w = truncateWeights (zipWith (+) w (mkNormals' (0,sigma) seed))

objective :: Problem -> Solution -> Double
objective training w = fromIntegral hits / fromIntegral (length training)
  where
    hits :: Int
    hits = sum $ oneOut (knnHit w) training

localsearchstep :: (RandomGen g) => g -> Int -> Problem -> Solution -> Maybe Solution
localsearchstep gen searchBound training w =
    find (\w' -> objective training w < objective training w')
    $ take searchBound
    $ map (($ w) . mutation 0.3) (randoms gen)

localsearch :: (RandomGen g) => g -> Int -> Int -> Problem -> Solution -> Solution
localsearch gen iters searchBound training = acc gen iters
  where
    acc :: (RandomGen g) => g -> Int -> Solution -> Solution
    acc _    0 s = s
    acc gen' n s = case localsearchstep gen' searchBound training s of
                     Just s' -> acc ((snd . next) gen') (n-1) s'
                     Nothing -> s


localsearchMetaheuristic :: (RandomGen g) => g -> Int -> Problem -> Solution
localsearchMetaheuristic gen leng s = localsearch gen 100 100 s (replicate leng 0)

localSearch :: (RandomGen g) => g -> Problem -> Solution
localSearch gen dataset = localsearchMetaheuristic gen (nAttr dataset) dataset



data MainFlags = MainFlags
  { flagSeed :: Int
  , flagKnn :: Bool
  , flagRelief :: Bool
  , flagLocalSearch :: Bool
  }

instance Options MainFlags where
  -- Definición de las distintas flags que puede tomar el programa
  -- como entrada.
  defineOptions = pure MainFlags
    <*> simpleOption "seed"    1     "Semilla de generación aleatoria"
    <*> simpleOption "1nn"     False "1-knn con distancia euclídea"
    <*> simpleOption "relief"  False "Algoritmo RELIEF"
    <*> simpleOption "bl"      False "Búsqueda local"


-- INPUT
main :: IO ()
main = runCommand $ \opts args ->
  case args of
    [filename] -> do
      dataset <- normalizeDataset <$> readArff filename
      let g = mkStdGen (flagSeed opts)
      let algorithm
            | flagKnn opts = knnTrivial
            | flagRelief opts = relief
            | flagLocalSearch opts = localSearch g 
            | otherwise = knnTrivial
      let algorithmName
            | flagKnn opts = "1NN con distancia euclídea"
            | flagRelief opts = "el algoritmo RELIEF"
            | flagLocalSearch opts = "búsqueda local"
            | otherwise = "1NN con distancia euclídea"
      let algorithmAbrv
            | flagKnn opts = "1nn"
            | flagRelief opts = "relief"
            | flagLocalSearch opts = "bl"
            | otherwise = "1nn"           

      putStrLn $ "Calcula " ++ algorithmName ++ " sobre " ++ filename ++ "..."
      printReport algorithm dataset
      putStrLn $ "Guarda las soluciones en " ++ filename ++ "." ++ algorithmAbrv ++ ".out"
      return ()

    _ -> putStrLn "El programa debe tomar como argumento un archivo"
  
