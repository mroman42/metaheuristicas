module Aggca where

import Genetic
import Individual (arithcross')
import TemplateMain

main :: IO ()
main = templateMain $ genetic (generationalStep arithcross')
