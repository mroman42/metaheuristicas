module AmNew where

import Genetic
import Individual (twoint, blx'', blx')
import TemplateMain

main :: IO ()
main = templateMain $ memetic (generationalStep' blx'') memeticStepAll

