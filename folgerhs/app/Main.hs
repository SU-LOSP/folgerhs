module Main (main) where

import Control.Monad
import System.Environment

import Text.XML.Light.Input (parseXML)

import Folgerhs.Stage


main :: IO ()
main = do args <- getArgs
          guard (length args == 1)
          source <- readFile $ head args
          let contents = parseXML source
              results = states (corpus contents) beginning
          mapM_ print results
          return ()
