module Folgerhs.Stage ( corpus
                    , beginning
                    , states
                    ) where

import Data.List
import Control.Monad

import Text.XML.Light.Proc (onlyElems)
import Text.XML.Light.Types (Content, Element)

import Folgerhs.Utils


type Line = String
type Character = String
type State = (Line, Character, [Character])

corpus :: [Content] -> [Element]
corpus = concatMap descendants . drillTagPath ["TEI", "text", "body"] . onlyElems

beginning :: State
beginning = ("0", "", [])

setLine :: Line -> State -> State
setLine n' (n, s, cs) = (n', s, cs)

setSpeaker :: Character -> State -> State
setSpeaker s' (n, s, cs) = (n, s', cs)

stageEntrance :: String -> State -> State
stageEntrance crepr' (n, s, cs) = let cs' = words crepr'
                                   in (n, s, nub (cs' ++ cs))

stageExit :: String -> State -> State
stageExit crepr' (n, s, cs) = let cs' = words crepr'
                               in (n, s, cs \\ cs')

state :: Element -> State -> Maybe State
state el st
  | isTag "milestone" el = case (attr "unit" el, attr "n" el) of
                             (Just "ftln", Just n) -> return $ setLine n st
                             _ -> Nothing
  | isTag "sp" el = case attr "who" el of
                      Just s -> return $ setSpeaker s st
                      _ -> Nothing
  | isTag "stage" el = case (attr "type" el, attr "who" el) of
                         (Just "entrance", Just cs) -> return $ stageEntrance cs st
                         (Just "exit", Just cs) -> return $ stageExit cs st
                         _ -> Nothing
  | otherwise = Nothing

states :: [Element] -> State -> [State]
states [] st = [st]
states (e:es) st = case state e st of
                     Just st' -> st : states es st'
                     Nothing -> states es st
