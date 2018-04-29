module Ageblx where

import Genetic
import Individual (blx)
import TemplateMain

main :: IO ()
main = templateMain $ genetic (stationaryStep blx)
 
