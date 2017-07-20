module Folgerhs.Presence (presence) where

import Control.Monad
import Data.List

import Folgerhs.Stage
import Folgerhs.Parse (parse)
import Folgerhs.Display


displayRow :: [Character] -> Stage -> String
displayRow gcs (l, s, cs) = l ++ "," ++ intercalate "," (map (displayPresence s cs) gcs)

displayHeader :: [Character] -> String
displayHeader gcs = "Act.Scene.Line," ++ intercalate "," (map displayCharacter gcs)

presence :: FilePath -> Float -> IO ()
presence f relevance = do source <- readFile f
                          let stages = parse source
                              gcs = characters stages
                          putStrLn $ displayHeader gcs
                          mapM_ (putStrLn . displayRow gcs) stages
                          return ()
