module Folgerhs.Presence (presence) where

import Control.Monad
import Data.List
import Data.Bool
import Data.Maybe

import Text.XML.Light.Input (parseXML)

import Folgerhs.Stage


displayState :: [Character] -> State -> String
displayState gcs (l, s, cs) = l ++ "," ++ s ++ "," ++ displayStageChar gcs cs
    where displayStageChar gcs cs = intercalate "," $ map (bool "0" "1" . (`elem` cs)) gcs

displayStage :: Character -> [Character] -> Character -> String
displayStage s cs c = if s == c
                         then "Speaker"
                         else bool "Absent" "Present" (elem c cs)

displayRow :: [Character] -> State -> String
displayRow gcs (l, s, cs) = l ++ "," ++ intercalate "," (map (displayStage s cs) gcs)

displayCharacter :: Character -> String
displayCharacter c = let n = fromMaybe c (stripPrefix "#" c)
                      in case span (/= '_') $ reverse n of
                           ("", p) -> p
                           (s, "") -> s
                           (s, p) -> reverse $ tail p

displayHeader :: [Character] -> String
displayHeader gcs = "Act.Scene.Line," ++ intercalate "," (map displayCharacter gcs)

characters :: [State] -> [Character]
characters = nub . concatMap (\(_, _, cs) -> cs)

presence :: FilePath -> Float -> IO ()
presence f relevance = do source <- readFile f
                          let contents = parseXML source
                              results = states (corpus contents) beginning
                              gcs = characters results
                          putStrLn $ displayHeader gcs
                          mapM_ (putStrLn . displayRow gcs) results
                          return ()
