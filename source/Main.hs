-- import Data.Array.Accelerate
import System.IO
import Text.Read
import Data.List
import Data.Ord
import Data.List.Split
import Control.Arrow

type Class = Float
type Value = Float
type Weight = Float
type UnWeight = Float
type Distance = Float

type ClassifiedInstance = ([Value], Class)
type Instance = [Value]
type Subset = [ClassifiedInstance]
type Solution = [Weight]
data Dataset = Dataset
  { attrs :: [String] 
  , instances :: [ClassifiedInstance]
  } deriving (Show)

-- dist :: [Value] -> [Value] -> Distance
-- dist a b = sum $ map (** 2.0) (zipWith (-) a b)

-- knn :: [ClassifiedInstance] -> Instance -> Class
-- knn ins a = snd $ minimumBy (comparing fst) $ map (first (dist a)) ins


-- | Weighed distance between two instances.
distw :: [Weight] -> Instance -> Instance -> Distance
distw w a b = sum $ zipWith (*) w' $ map (** 2.0) (zipWith (-) a b)
  where
    -- Weights less than 0.2 are discarded
    w' :: [Weight]
    w' = map (\x -> if x < 0.2 then 0 else x) w


distc :: [Weight] -> ClassifiedInstance -> ClassifiedInstance -> Distance
distc w a b = distw w (fst a) (fst b)

knnw :: [Weight] -> [ClassifiedInstance] -> Instance -> Class
knnw w ins a = snd $ minimumBy (comparing fst) $ map (first (distw w a)) ins

distClosest :: [Weight] -> Instance -> [Instance] -> Distance
distClosest w a ins = minimum $ map (distw w a) ins




-- Greedy: RELIEF
ofClass :: Bool -> Class -> [ClassifiedInstance] -> [ClassifiedInstance]
ofClass True c ins = filter (\x-> snd x == c) ins
ofClass False c ins = filter (\x-> snd x /= c) ins

friendsOf :: ClassifiedInstance -> [ClassifiedInstance] -> [ClassifiedInstance]
friendsOf a = ofClass True (snd a)
enemiesOf :: ClassifiedInstance -> [ClassifiedInstance] -> [ClassifiedInstance]
enemiesOf a = ofClass False (snd a)

closestTo :: [Weight] -> ClassifiedInstance -> [ClassifiedInstance] -> ClassifiedInstance
closestTo w a = minimumBy (comparing (distc w a))
closestFriend :: [Weight] -> ClassifiedInstance -> [ClassifiedInstance] -> ClassifiedInstance
closestFriend w a ins = closestTo w a $ friendsOf a ins
closestEnemy :: [Weight] -> ClassifiedInstance -> [ClassifiedInstance] -> ClassifiedInstance
closestEnemy w a ins = closestTo w a $ enemiesOf a ins

-- distClosestFriendIn :: [Weight] -> [ClassifiedInstance] -> ClassifiedInstance -> ClassifiedInstance
-- distClosestFriendIn w ins a = distClosest w (fst a) $ friendsOf a ins

-- distClosestEnemyIn :: [Weight] -> [ClassifiedInstance] -> ClassifiedInstance -> Distance
-- distClosestEnemyIn w ins a = distClosest w (fst a) $ enemiesOf a ins

normalize :: [UnWeight] -> [Weight]
normalize w = map ((/ maximum w) . negtrunc) w
  where
    negtrunc :: UnWeight -> UnWeight
    negtrunc a = if a < 0 then 0 else a

reliefUpdateWith :: [Weight] -> [ClassifiedInstance] -> ClassifiedInstance -> [Weight]
reliefUpdateWith w ins a = normalize $ zipWith (+) (zipWith (-) w dimb) dimc
  where
    b = closestFriend w a ins
    c = closestEnemy w a ins
    dimb = map abs (zipWith (-) (fst a) (fst b))
    dimc = map abs (zipWith (-) (fst a) (fst c))

relief' :: [Weight] -> [ClassifiedInstance] -> [Weight]
relief' w ins = (\(v,_,_) -> v) $ acc (w,[],ins)
  where
    acc :: ([Weight],[ClassifiedInstance],[ClassifiedInstance])
        -> ([Weight],[ClassifiedInstance],[ClassifiedInstance])
    acc (v,_,[]) = (v,[],[])
    acc (v,l,a:r) = acc (reliefUpdateWith v (l ++ r) a, a:l , r)

relief :: Subset -> Solution
relief = relief' [0,0..]




-- tasa_clas
-- TODO: trabaja ahora mismo sólo sobre el test y no usa el training también
validate :: [Weight] -> Subset -> Subset -> Float
validate w training test = hits / fromIntegral (length test)
  where
    hits :: Float
    hits = sum $ map ((\(x,y) -> if x == y then 0 else 1) . (knnw w training . fst &&& snd)) test

-- tasa_red
simplicity :: [Weight] -> Float
simplicity w = fromIntegral (length (filter (< 0.2) w)) / fromIntegral (length w)

alpha :: Float
alpha = 0.5

agreggate :: Solution -> Subset -> Subset -> Float
agreggate w training test = alpha * validate w training test + (1 - alpha) * simplicity w
      



-- | Splits a subset into 5, roughly equal and balanced subsets.
fivesplit :: Subset -> [Subset]
fivesplit a = zipWith (++) chunks1 chunks2
  where
    class1 = filter (\u -> snd u == 1.0) a
    class2 = filter (\u -> snd u == 2.0) a
    clen = quot (length a) 5 + if rem (length a) 5 == 0 then 0 else 1
    chunks1 = chunksOf clen class1
    chunks2 = chunksOf clen class2


-- | Average of 5 executions of the heuristic on the dataset.
fivefold :: (Subset -> Solution) -> Dataset -> Float
fivefold heuristic dataset = (\(x,_,_)->x) $ acc (0,[],datasplit)
  where
    datasplit = fivesplit (instances dataset)

    acc :: (Float,[Subset],[Subset]) -> (Float,[Subset],[Subset])
    acc (f,_,[]) = (f,[],[])
    acc (f,l,s:r) = (f + agreggate (heuristic (concat (l++r))) (concat (l++r)) s, s:l, r)




-- INPUT
type Filename = String
fileOzone, fileParkinson, fileHeart :: Filename
fileOzone = "./apc/ozone-320.arff"
fileParkinson = "./apc/parkinsons.arff"
fileHeart = "./apc/spectf-heart.arff"

readArff :: Filename -> IO Dataset
readArff filename = do
  handle <- openFile filename ReadMode
  contents <- tail . tail . lines <$> hGetContents handle
  let secAttrClass = takeWhile (isPrefixOf "@attribute") contents
  let secAttr = init secAttrClass
  -- let secClass = last secAttrClass
  let secData = dropWhile (\ s -> isPrefixOf "@" s || s == "") contents
  let attributes = map (reverse . drop (length " numeric") . reverse . drop (length "@attribute")) secAttr
  let rawdata = map (map (check . (id &&& readMaybe)) . splitOn ",") secData :: [[Value]]
  let values = map (init &&& last) rawdata
  return (Dataset attributes values)
    where
      check :: (String , Maybe Float) -> Float
      check (_,Just f) = f
      check (x,Nothing) = error ("Error reading ->" ++ x ++ "<-")

main :: IO ()
main = do
  datasetOzone <- readArff fileOzone
  datasetParkinson <- readArff fileParkinson
  datasetHeart <- readArff fileHeart

  print $ map (fivefold relief) [datasetOzone,datasetParkinson,datasetHeart]
  
  return ()

-- Input
-- Element = (Data, Class)
-- Solution = Vector Float

