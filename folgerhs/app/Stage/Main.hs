module Main (main) where

import Control.Monad
import System.Environment
import Data.List
import Data.Bool

import Text.XML.Light.Input (parseXML)

import Folgerhs.Stage


displayState :: [Character] -> State -> String
displayState gcs (l, s, cs) = l ++ "," ++ s ++ "," ++ displayStageChar gcs cs
    where displayStageChar gcs cs = intercalate "," $ map (bool "0" "1" . (`elem` cs)) gcs

displayHeader :: [Character] -> String
displayHeader gcs = "line,speaker," ++ intercalate "," gcs

characters :: [State] -> [Character]
characters = nub . concatMap (\(_, _, cs) -> cs)

main :: IO ()
main = do args <- getArgs
          if length args /= 1
             then error "Expecting filename."
             else do source <- readFile $ head args
                     let contents = parseXML source
                         results = states (corpus contents) beginning
                         gcs = characters results
                     putStrLn $ displayHeader gcs
                     mapM_ (putStrLn . displayState gcs) results
                     return ()
