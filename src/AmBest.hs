module AmBest where

import Genetic
import Individual (blx')
import TemplateMain

main :: IO ()
main = templateMain $ memetic (stationaryStep blx') memeticStepBest
