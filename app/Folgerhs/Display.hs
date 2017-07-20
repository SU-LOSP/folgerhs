module Folgerhs.Display where

import Data.Maybe
import Data.List
import Data.Bool

import Folgerhs.Stage


displayCharacter :: Character -> String
displayCharacter c = let n = fromMaybe c (stripPrefix "#" c)
                      in case span (/= '_') $ reverse n of
                           ("", p) -> p
                           (s, "") -> s
                           (s, p) -> reverse $ tail p

displayPresence :: Character -> [Character] -> Character -> String
displayPresence s cs c = if s == c
                            then "Speaker"
                            else bool "Absent" "Present" (elem c cs)
