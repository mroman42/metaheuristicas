module Normal where

import Control.Monad.Random
import Data.Random.Normal

getNormal :: (Double,Double) -> Rand StdGen Double
getNormal (mean,sdev) = head . mkNormals' (mean,sdev) <$> getRandom
