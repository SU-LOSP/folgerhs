module Folgerhs.Protagonism (protagonism) where

import Control.Monad
import Data.Function (on)
import Data.List

import Text.XML.Light.Input (parseXML)
import Text.Printf (printf)

import Folgerhs.Stage

speaker :: State -> String
speaker (_, s, _) = s

count :: Eq a => a -> [a] -> Int
count e = length . filter (e ==)

sortedProtagonism :: [String] -> [(String, Float)]
sortedProtagonism ss = sortBy (flip $ on compare snd) $ map (`charProt` ss) (nub ss)
    where charProt c ss = (c, fromIntegral (count c ss) / fromIntegral (length ss))

reprProt :: (String, Float) -> String
reprProt (c, r) = printf "%.2f%% \t %s" (r*100) c

protagonism :: FilePath -> IO ()
protagonism f = do source <- readFile f
                   let contents = parseXML source
                       speakers = map speaker $ states (corpus contents) beginning
                   mapM_ (putStrLn . reprProt) (sortedProtagonism speakers)
                   return ()
