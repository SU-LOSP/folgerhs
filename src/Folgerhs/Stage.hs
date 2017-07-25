module Folgerhs.Stage where

import Data.Maybe
import Data.List

type Line = String
type Character = String
data StageEvent = Milestone Line
                | Entrance [Character]
                | Exit [Character]
                | Speech Character
                deriving (Eq, Show)

onStage :: [Character] -> StageEvent -> [Character]
onStage chs (Entrance chs') = nub (chs ++ chs')
onStage chs (Exit chs') = chs \\ chs'
onStage chs _ = chs

speaker :: Character -> StageEvent -> Character
speaker _ (Speech ch') = ch'
speaker ch _ = ch

maybeSpeaker :: StageEvent -> Maybe Character
maybeSpeaker (Speech ch) = Just ch
maybeSpeaker _ = Nothing

line :: Line -> StageEvent -> Line
line _ (Milestone l') = l'
line l _ = l

maybeLine :: StageEvent -> Maybe Line
maybeLine (Milestone l) = Just l
maybeLine _ = Nothing

lines :: [StageEvent] -> [Line]
lines = mapMaybe maybeLine

isLine :: Line -> StageEvent -> Bool
isLine l = maybe False ((==) l) . maybeLine

seek :: Line -> [StageEvent] -> [StageEvent]
seek "" = id
seek l = dropWhile (not . isLine l)

accumStage :: [StageEvent] -> [Character]
accumStage = foldl onStage []

lineStage :: Line -> [StageEvent] -> [Character]
lineStage l = accumStage . takeWhile (not . isLine l)

accumSpeaker :: [StageEvent] -> Character
accumSpeaker = foldl speaker ""

lineSpeaker :: Line -> [StageEvent] -> Character
lineSpeaker l = accumSpeaker . takeWhile (not . isLine l)

characters :: [StageEvent] -> [Character]
characters [] = []
characters (se:ses) = case se of
                        Entrance chs -> nub $ chs ++ characters ses
                        _ -> characters ses

selectCharacters :: (Character -> Bool) -> [StageEvent] -> [StageEvent]
selectCharacters _ [] = []
selectCharacters f (se:ses) = let r = selectCharacters f ses
                               in case se of
                                Entrance chs -> Entrance (filter f chs) : r
                                Exit chs -> Exit (filter f chs) : r
                                Speech ch -> if f ch then se : r else r
                                se -> se : r
