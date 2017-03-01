module Main (main) where

import Control.Monad
import System.Environment
import Data.Function (on)
import Data.List

import Text.XML.Light.Input (parseXML)
import Text.Printf (printf)

import Folgerhs.Stage

speaker :: State -> String
speaker (_, s, _) = s

count :: Eq a => a -> [a] -> Int
count e = length . filter (e ==)

protagonism :: [String] -> [(String, Float)]
protagonism ss = sortBy (flip $ on compare snd) $ map (`charProt` ss) (nub ss)
    where charProt c ss = (c, fromIntegral (count c ss) / fromIntegral (length ss))

reprProt :: (String, Float) -> String
reprProt (c, r) = printf "%.2f%% \t %s" (r*100) c

main :: IO ()
main = do args <- getArgs
          if length args /= 1
             then error "Expecting filename."
             else do source <- readFile $ head args
                     let contents = parseXML source
                         speakers = map speaker $ states (corpus contents) beginning
                     mapM_ (putStrLn . reprProt) (protagonism speakers)
                     return ()
