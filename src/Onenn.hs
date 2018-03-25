module Onenn where

import Base
import TemplateMain

onenn :: Seed -> Problem -> Solution
onenn _ p = replicate (nAttr p) 1

main :: IO ()
main = templateMain onenn
