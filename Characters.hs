module Characters () where

import Data.Maybe
import Control.Monad

import Text.XML.Light.Input (parseXML)
import Text.XML.Light.Proc (findElements, onlyElems, elChildren)
import Text.XML.Light.Types (QName (..), Element (..), Content, Attr (..) )


filename :: String
filename = "FolgerDigitalTexts_XML_Complete/FolgerDigitalTexts_XML_Complete/Oth.xml"

hasName :: String -> Element -> Bool
hasName n = (==) n . qName . elName

drillNamePath :: [String] -> [Element] -> [Element]
drillNamePath [] = id
drillNamePath (n:ns) = drillNamePath ns . concatMap elChildren . filter (hasName n)

body :: [Content] -> [Element]
body = drillNamePath ["TEI", "text", "body"] . onlyElems

attr :: String -> Element -> Maybe String
attr n = listToMaybe . map attrVal . filter ((==) n . qName . attrKey) . elAttribs

type Act = (String, [Scene])
type Scene = (String, [Element])

scene :: Element -> Maybe Scene
scene e = do guard (hasName "div2" e)
             name <- attr "n" e
             return (name, elChildren e)

act :: Element -> Maybe Act
act e = do guard (hasName "div1" e)
           name <- attr "n" e
           return (name, mapMaybe scene (elChildren e))

acts :: [Element] -> [Act]
acts = mapMaybe act

main :: IO ()
main = do source <- readFile filename
          let contents = parseXML source
          print ((acts $ body contents) !! 0)
