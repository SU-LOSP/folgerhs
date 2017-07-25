module Folgerhs.Protagonism (protagonism) where

import Data.Function (on)
import Data.List
import Data.Maybe

import Text.Printf (printf)

import Folgerhs.Stage
import Folgerhs.Parse (parse)


sortedPercent :: [Character] -> [(Character, Float)]
sortedPercent chs = let perCh = [ (head g, g % chs) | g <- group (sort chs) ]
                     in sortBy (on compare snd) perCh
                    where (%) a b = on (/) (fromIntegral . length) a b
                            
reprProt :: (Character, Float) -> String
reprProt (c, r) = printf "%.2f%% \t %s" (r*100) c

protagonism :: FilePath -> IO ()
protagonism f = do source <- readFile f
                   let sps = mapMaybe speaker (parse source)
                   mapM_ (putStrLn . reprProt) (sortedPercent sps)
                   return ()
