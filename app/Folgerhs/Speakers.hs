module Folgerhs.Speakers (speakers) where

import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Ord

import Text.Printf (printf)

import Folgerhs.Stage
import Folgerhs.Parse (parse)


sortedPercent :: [Character] -> [(Character, Float)]
sortedPercent chs = let perCh = [ (head g, g % chs) | g <- group (sort chs) ]
                     in sortBy (flip $ comparing snd) perCh
                    where (%) a b = on (/) (fromIntegral . length) a b
                            
reprProt :: (Character, Float) -> String
reprProt (c, r) = printf "%.2f%% \t %s" (r*100) c

speakers :: FilePath -> IO ()
speakers f = do source <- readFile f
                let sps = mapMaybe maybeSpeaker (parse source)
                mapM_ (putStrLn . reprProt) (sortedPercent sps)
                return ()
