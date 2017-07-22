module Folgerhs.Stage where

import Data.List ((\\), nub)
import Control.Monad


type Line = String
type Character = String
type Group = [Character]
type Stage = (Line, Character, Group)

line :: Stage -> Line
line (l, _, _) = l

speaker :: Stage -> String
speaker (_, s, _) = s

present :: Stage -> Group
present (_, _, g) = g

setLine :: Line -> Stage -> Stage
setLine n' (n, s, cs) = (n', s, cs)

setSpeaker :: Character -> Stage -> Stage
setSpeaker s' (n, s, cs) = (n, s', cs)

entrance :: Character -> Stage -> Stage
entrance c (n, s, cs) = (n, s, nub (c:cs))

exit :: Character -> Stage -> Stage
exit c (n, s, cs) = (n, s, cs \\ [c])

characters :: [Stage] -> [Character]
characters = nub . concatMap (\(_, _, cs) -> cs)

perLine :: [Stage] -> [Stage]
perLine [] = []
perLine (s:[]) = [s]
perLine (s:s':ss)
  | line s == line s' = perLine (s':ss)
  | otherwise = s : perLine (s':ss)

lines :: [Stage] -> [Line]
lines = map (\(l, _, _) -> l)

group :: [Group] -> [Character] -> [Group]
group gs cs = cs : [ g \\ cs | g <- gs]

groups :: [[Character]] -> [[Group]]
groups = tail . scanl group []
