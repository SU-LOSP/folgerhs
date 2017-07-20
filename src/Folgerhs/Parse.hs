module Folgerhs.Parse ( parseCorpus
                      , corpus
                      , beginning
                      , parse
                      ) where

import Data.Maybe
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

beginning :: Stage
beginning = ("0", "", [])

parseElement :: Element -> Stage -> Maybe Stage
parseElement el st
  | isTag "milestone" el = case (attr "unit" el, attr "n" el) of
                             (Just "ftln", Just n) -> return $ setLine n st
                             _ -> Nothing
  | isTag "sp" el = case attr "who" el of
                      Just s -> return $ setSpeaker s st
                      _ -> Nothing
  | isTag "stage" el = case (attr "type" el, attr "who" el) of
                         (Just "entrance", Just cs) -> return $ foldr entrance st (words cs)
                         (Just "exit", Just cs) -> return $ foldr exit st (words cs)
                         _ -> Nothing
  | otherwise = Nothing

parseCorpus :: [Element] -> Stage -> [Stage]
parseCorpus [] st = [st]
parseCorpus (e:es) st = case parseElement e st of
                          Just st' -> st : parseCorpus es st'
                          Nothing -> parseCorpus es st

parse :: String -> [Stage]
parse input = let content = parseXML input
               in parseCorpus (corpus content) beginning
