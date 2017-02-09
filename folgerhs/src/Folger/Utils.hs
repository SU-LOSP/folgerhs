module Folger.Utils ( isTag
                    , drillTagPath
                    , attr
                    , descendants
                    ) where

import Data.Maybe

import Text.XML.Light.Proc (elChildren)
import Text.XML.Light.Types (QName (..), Element (..), Content, Attr (..) )


isTag :: String -> Element -> Bool
isTag n = (==) n . qName . elName

drillTagPath :: [String] -> [Element] -> [Element]
drillTagPath [] = id
drillTagPath (n:ns) = drillTagPath ns . concatMap elChildren . filter (isTag n)

attr :: String -> Element -> Maybe String
attr n = listToMaybe . map attrVal . filter ((==) n . qName . attrKey) . elAttribs

descendants :: Element -> [Element]
descendants e = e : concatMap descendants (elChildren e)
