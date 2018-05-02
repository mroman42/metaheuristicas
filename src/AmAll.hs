module AmAll where

import Genetic
import Individual (blx')
import TemplateMain

main :: IO ()
main = templateMain $ memetic (generationalStep blx') memeticStepAll
