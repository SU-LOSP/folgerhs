module Folgerhs.Presence (presence) where

import Control.Monad
import Data.List
import Data.Bool

import Folgerhs.Stage as S
import Folgerhs.Parse (parse)

type Row = (Line, [String])

rows :: [Character] -> [StageEvent] -> [Row]
rows chs ses = [ (l, [ presence l ch | ch <- chs ]) | l <- S.lines ses ]
    where presence l ch = if lineSpeaker l ses == ch 
                             then "Speaker"
                             else bool "Absent" "Present" $
                                 (elem ch $ lineStage l ses)

displayRow :: Row -> String
displayRow (l, ss) = l ++ "," ++ intercalate "," ss

displayHeader :: [Character] -> String
displayHeader chs = "Act.Scene.Line," ++ intercalate "," chs

presence :: FilePath -> Bool -> IO ()
presence f wu = do source <- readFile f
                   let scf = if wu then hasName else const True
                       ses = selectCharacters scf $ parse source
                       chs = characters ses
                   putStrLn $ displayHeader chs
                   mapM_ (putStrLn . displayRow) (rows chs ses)
                   return ()
