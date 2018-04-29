module Ageca where

import Genetic
import Individual (arithcross')
import TemplateMain

main :: IO ()
main = templateMain $ genetic (stationaryStep arithcross')
