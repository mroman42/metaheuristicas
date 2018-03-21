-- import Data.Array.Accelerate
import System.IO
import Text.Read
import Data.List
import Data.Ord
-- import Data.Maybe
import Data.List.Split
import Control.Arrow
import Data.Random.Normal
import System.Random

import Base


-- Leave one out
oneOut :: [a] -> ([a] -> a -> b) -> [b]
oneOut list f = map (uncurry f) (acc list [])
  where
    acc :: [a] -> [a] -> [([a],a)]
    acc _ [] = []
    acc l (a:r) = (l ++ r, a) : acc (a:l) r







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
    hits = sum $ oneOut training (knnHit w)

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


-- tasa_clas
validate :: [Weight] -> Problem -> Problem -> Double
validate w training test = fromIntegral hits / fromIntegral (length test)
  where
    hits :: Int
    hits = sum $ map (knnHit w training) test

-- tasa_red
simplicity :: [Weight] -> Double
simplicity w = fromIntegral (length (filter (< 0.2) w)) / fromIntegral (length w)

alpha :: Double
alpha = 0.5

score :: Solution -> Problem -> Problem -> Double
score w training test = alpha * validate w training test + (1 - alpha) * simplicity w


-- | Splits a subset into five, roughly equal and balanced subsets.
fivesplit :: Problem -> [Problem]
fivesplit a = zipWith (++) chunks1 chunks2
  where
    class1 = filter (\u -> snd u == 1.0) a
    class2 = filter (\u -> snd u == 2.0) a
    clen = quot (length a) 5 + if rem (length a) 5 == 0 then 0 else 1
    chunks1 = chunksOf clen class1
    chunks2 = chunksOf clen class2


-- | Average of 5 executions of the heuristic on the dataset.
fivefold :: (Problem -> Solution) -> Problem -> Double
fivefold heuristic dataset = (\(x,_,_)->x) $ acc (0,[],datasplit)
  where
    datasplit = fivesplit dataset

    acc :: (Double,[Problem],[Problem]) -> (Double,[Problem],[Problem])
    acc (f,_,[]) = (f,[],[])
    acc (f,l,s:r) = (f + score (heuristic (concat (l++r))) (concat (l++r)) s, s:l, r)




-- INPUT
type Filename = String
fileOzone, fileParkinson, fileHeart :: Filename
fileOzone = "./apc/ozone-320.arff"
fileParkinson = "./apc/parkinsons.arff"
fileHeart = "./apc/spectf-heart.arff"

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

main :: IO ()
main = do
  datasetOzone <- readArff fileOzone
  datasetParkinson <- readArff fileParkinson
  datasetHeart <- readArff fileHeart

  g <- getStdGen
  print $ map (fivefold relief . normalizeDataset) [datasetOzone,datasetParkinson,datasetHeart]
  print $ map (fivefold (localsearchMetaheuristic g (nAttr datasetOzone)) . normalizeDataset) [datasetOzone, datasetParkinson, datasetHeart]
--  print $ map (fivefold (localsearchMetaheuristic g) . normalizeDataset) [datasetOzone]
  
  return ()
