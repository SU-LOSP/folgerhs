module Folgerhs.Parse ( parseCorpus
                      , corpus
                      , parse
                      ) where

import Data.Maybe
import Data.List
import Text.XML.Light.Input (parseXML)
import Text.XML.Light.Proc (onlyElems, elChildren)
import Text.XML.Light.Types (QName (..), Element (..), Content, Attr (..) )

import Folgerhs.Stage

isTag :: String -> Element -> Bool
isTag n = (==) n . qName . elName

drillTagPath :: [String] -> [Element] -> [Element]
drillTagPath [] = id
drillTagPath (n:ns) = drillTagPath ns . concatMap elChildren . filter (isTag n)

attr :: String -> Element -> Maybe String
attr n = listToMaybe . map attrVal . filter ((==) n . qName . attrKey) . elAttribs

descendants :: Element -> [Element]
descendants e = e : concatMap descendants (elChildren e)

corpus :: [Content] -> [Element]
corpus = concatMap descendants . drillTagPath ["TEI", "text", "body"] . onlyElems

charName :: String -> Character
charName c = let n = fromMaybe c (stripPrefix "#" c)
              in case span (/= '_') $ reverse n of
                   ("", p) -> p
                   (s, "") -> s
                   (s, p) -> reverse $ tail p

parseElement :: Element -> Maybe StageEvent
parseElement el
  | isTag "milestone" el = case (attr "unit" el, attr "n" el) of
                             (Just "ftln", Just n) -> Just $ Milestone n 
                             _ -> Nothing
  | isTag "sp" el = case attr "who" el of
                      Just s -> Just $ Speech s
                      _ -> Nothing
  | isTag "stage" el = case (attr "type" el, attr "who" el) of
                         (Just "entrance", Just cs) -> Just $ Entrance (map charName $ words cs)
                         (Just "exit", Just cs) -> Just $ Exit (map charName $ words cs)
                         _ -> Nothing
  | otherwise = Nothing

parseCorpus :: [Element] -> [StageEvent]
parseCorpus [] = []
parseCorpus (e:es) = case parseElement e of
                        Just se -> se : parseCorpus es
                        Nothing -> parseCorpus es

parse :: String -> [StageEvent]
parse input = let content = parseXML input
               in parseCorpus (corpus content)
