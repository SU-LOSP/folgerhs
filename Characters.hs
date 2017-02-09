module Characters () where

import Data.Maybe
import Data.List
import Control.Monad

import Text.XML.Light.Input (parseXML)
import Text.XML.Light.Proc (findElements, onlyElems, elChildren)
import Text.XML.Light.Types (QName (..), Element (..), Content, Attr (..) )


filename :: String
filename = "FolgerDigitalTexts_XML_Complete/FolgerDigitalTexts_XML_Complete/Oth.xml"

isTag :: String -> Element -> Bool
isTag n = (==) n . qName . elName

drillTagPath :: [String] -> [Element] -> [Element]
drillTagPath [] = id
drillTagPath (n:ns) = drillTagPath ns . concatMap elChildren . filter (isTag n)

body :: [Content] -> [Element]
body = drillTagPath ["TEI", "text", "body"] . onlyElems

attr :: String -> Element -> Maybe String
attr n = listToMaybe . map attrVal . filter ((==) n . qName . attrKey) . elAttribs

descendants :: Element -> [Element]
descendants e = e : concatMap descendants (elChildren e)

type Line = String
type Character = String
type State = (Line, Character, [Character])

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

main :: IO ()
main = do source <- readFile filename
          let contents = parseXML source
              corpus = concatMap descendants (body contents)
              results = states corpus beginning
          mapM_ print results
          return ()
