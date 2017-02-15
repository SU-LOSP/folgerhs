module Main (main) where

import Control.Monad
import System.Environment

import Text.XML.Light.Input (parseXML)

import Folgerhs.Stage

displayState :: State -> String
displayState (l, s, cs) = l ++ "\t" ++ s ++ "\t" ++ show cs

main :: IO ()
main = do args <- getArgs
          if length args /= 1
             then error "Expecting filename."
             else do source <- readFile $ head args
                     let contents = parseXML source
                         results = states (corpus contents) beginning
                     mapM_ (putStrLn . displayState) results
                     return ()
