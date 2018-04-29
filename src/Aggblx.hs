module Aggblx where

import Genetic
import Individual (blx')
import TemplateMain

main :: IO ()
main = templateMain $ genetic (generationalStep blx')
