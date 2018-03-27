module Relief where

import Data.List
import Data.Ord

import Base
import TemplateMain



-- Filtra a los que son de la misma o distinta clase.
ofClass :: Bool -> Class -> Problem -> Problem
ofClass True  c = filter (\x -> snd x == c)
ofClass False c = filter (\x -> snd x /= c)
friendsOf :: Instance -> Problem -> Problem
friendsOf a = ofClass True (snd a)
enemiesOf :: Instance -> Problem -> Problem
enemiesOf a = ofClass False (snd a)

-- Busca al amigo y enemigo más cercanos en distancia Euclídea.
closestTo :: Instance -> [Instance] -> Instance
closestTo a = minimumBy (comparing (distSqrdEuclid a))

closestFriend :: Instance -> [Instance] -> Instance
closestFriend a ins = closestTo a $ friendsOf a ins
closestEnemy :: Instance -> [Instance] -> Instance
closestEnemy a ins = closestTo a $ enemiesOf a ins

-- Truncamiento de pesos
normalizeWeights :: [UnWeight] -> [Weight]
normalizeWeights w = map ((/ maximum w) . negtrunc) w
  where
    negtrunc :: UnWeight -> UnWeight
    negtrunc a = if a < 0 then 0 else a

-- Algoritmo RELIEF
relief :: Seed -> Problem -> Solution
relief _ p = normalizeWeights $ foldr (acc p) (zeroes p) p
  where
    acc :: [Instance] -> Instance -> Solution -> Solution
    acc q a s = zipWith (+) (zipWith (-) s dists_friend) dists_enemy
      where
        dists_friend = map abs (zipWith (-) (fst a) (fst (closestFriend a q)))
        dists_enemy  = map abs (zipWith (-) (fst a) (fst (closestEnemy  a q)))

    zeroes :: Problem -> Solution
    zeroes q = replicate (nAttr q) 0

main :: IO ()
main = templateMain relief
