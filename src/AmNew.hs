module AmNew where

import Genetic
import Individual (twoint)
import TemplateMain

main :: IO ()
main = templateMain $ memetic (generationalStep' twoint) memeticStepAll

