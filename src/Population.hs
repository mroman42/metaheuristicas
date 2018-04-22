module Population
  ( Population
  , size
  , nAttrPopl
  -- Reemplazamiento.
  , replaceWorstBy
  -- Generación aleatoria.
  , initialPopulation
  -- Escoger individuos.
  , binaryTournament
  )
where

import qualified Data.Set as T
import Data.List as L
import System.Random
import Individual as I
import Base (Problem, nAttr)

type Population = T.Set Individual

-- Tamaño de la población
size :: Population -> Int
size = T.size

-- Número de atributos de los individuos de la población
nAttrPopl :: Population -> Int
nAttrPopl = I.nAttrInd . T.elemAt 0

-- Reemplaza el peor individuo de la población por uno dado.
replaceWorstBy :: Individual -> Population -> Population
replaceWorstBy iv = T.insert iv . T.deleteMin

-- Genera una población aleatoria.
initialPopulation :: Problem -> StdGen -> Population
initialPopulation training g = T.fromList $
  map (randomIndividual training . mkStdGen) $
  L.take 30 $
  randoms g

-- Binary tournament
binaryTournament :: StdGen -> Population -> Individual
binaryTournament g popl
  | fitness a < fitness b = a
  | otherwise             = b
  where
    [x,y] = L.take 2 $ randomRs (0,T.size popl-1) g
    a = T.elems popl !! x
    b = T.elems popl !! y
