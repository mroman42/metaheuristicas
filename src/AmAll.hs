module AmAll where

import Genetic
import Individual (arithcross')
import TemplateMain

main :: IO ()
main = templateMain $ memetic (stationaryStep arithcross') memeticStepAll