module Population
  ( Population
  , size
  , nAttrPopl
  , pFromList
  , pToList
  , diversity
  -- Reemplazamiento.
  , replaceWorstByIfNotMember  
  , replaceWorstBy
  , replaceBy
  -- Generación aleatoria.
  , randomPopulation
  -- Mutación
  , mutatePopulationN
  -- Escoger individuos.
  , binaryTournament
  , binaryTournamentsWoReplace
  , binaryTournamentsWtReplace
  , bestOf
  -- Búsqueda local
  , poplLocalSearchBest
  )
where

-- import Debug.Trace
import qualified Data.Set as T
import Control.Monad.Random
import Data.List (nub)
import Individual as I
import Base (Problem)

type Population = T.Set Individual

-- | Tamaño de la población
size :: Population -> Int
size = T.size

-- | Número de atributos de los individuos de la población
nAttrPopl :: Population -> Int
nAttrPopl = I.nAttrInd . T.elemAt 0

-- | Reemplaza el peor individuo de la población por uno dado.
replaceWorstBy :: Individual -> Population -> Population
replaceWorstBy iv = T.deleteMin . T.insert iv

replaceWorstByIfNotMember :: Individual -> Population -> Population
replaceWorstByIfNotMember iv p
  | iv `elem` T.toList p = p
  | otherwise = replaceWorstBy iv p

-- | Incluye varios individuos en la población y elimina luego los
-- peores.
replaceBy :: [Individual] -> Population -> Population
replaceBy = foldr ((.) . replaceWorstBy) id

-- Torneo binario, que genera un individuo aleatorio en una población dada.
binaryTournament :: Population -> Rand StdGen Individual
binaryTournament p = do
  x <- getRandomR (0,size p - 1)
  y <- getRandomR (0,size p - 1)
  -- La población está ordenada, y cuanto mayor es el índice, mayor es
  -- la bondad del elemento.
  return $ T.elemAt (min x y) p

-- Múltiples torneos binarios sin reemplazamiento.
binaryTournamentsWoReplace :: Population -> Rand StdGen [Individual]
binaryTournamentsWoReplace p = do
  xs <- getRandomRs (0,size p - 1)
  ys <- getRandomRs (0,size p - 1)
  let zs = nub $ zipWith max xs ys
  return $ map (`T.elemAt` p) zs

-- Múltiples torneos binarios con reemplazamiento.
binaryTournamentsWtReplace :: Population -> Rand StdGen [Individual]
binaryTournamentsWtReplace p = do
  xs <- getRandomRs (0,size p - 1)
  ys <- getRandomRs (0,size p - 1)
  let zs = zipWith max xs ys
  return $ map (`T.elemAt` p) zs


-- Genera una población aleatoria de un número de individuos.
randomPopulation :: Int -> Problem -> Rand StdGen Population
randomPopulation n training = T.fromList <$> replicateM n (randomIndividual training)

-- | Aplica una mutación aleatoria en un individuo cualquiera de toda
-- la población.
mutatePopulation :: Problem -> Population -> Rand StdGen Population
mutatePopulation training p = do
  index <- getRandomR (0,size p-1)
  newIndv <- mutation training (T.elemAt index p)
  return $ T.insert newIndv $ T.deleteAt index p

-- | Aplica n mutaciones aleatorias sobre una población.
mutatePopulationN :: Int -> Problem -> Population -> Rand StdGen Population
mutatePopulationN n training = foldr (>=>) return (replicate n (mutatePopulation training))
  
-- | Aplica búsqueda local sobre los n mejores de la población, cada
-- uno con probabilidad fija. Devuelve las evaluaciones que ha
-- gastado.
poplLocalSearchBest :: Double -> Int -> Problem -> Population -> Rand StdGen (Population,Int)
poplLocalSearchBest prob n training popl = do
  let (other, elite) = T.splitAt (size popl - n) popl :: (Population , Population)
  localsearchs <- mapM (localSearchIn prob (2 * nAttrPopl popl) training) (T.toList elite)
  let melite = T.fromList $ map fst localsearchs
  let evals  = sum $ map snd localsearchs
  return (T.union other melite, evals)



diversity :: Population -> Double
diversity popl = sum [distance x y | x <- pToList popl, y <- pToList popl]

bestOf :: Population -> Individual
bestOf = T.findMax

pFromList :: [Individual] -> Population
pFromList = T.fromList

pToList :: Population -> [Individual]
pToList = T.toList
