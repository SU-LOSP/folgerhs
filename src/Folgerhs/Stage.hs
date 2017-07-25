module Folgerhs.Stage where

import Data.Maybe
import Data.List

type Line = String
type Character = String
data StageEvent = Milestone Line
                | Entrance [Character]
                | Exit [Character]
                | Speech Character

onStage :: [Character] -> StageEvent -> [Character]
onStage chs (Entrance chs') = nub (chs ++ chs')
onStage chs (Exit chs') = chs \\ chs'
onStage chs _ = chs

speaker :: StageEvent -> Maybe Character
speaker (Speech ch) = Just ch
speaker _ = Nothing

line :: StageEvent -> Maybe Line
line (Milestone l) = Just l
line _ = Nothing

lines :: [StageEvent] -> [Line]
lines = mapMaybe line

isLine :: Line -> StageEvent -> Bool
isLine l = maybe False ((==) l) . line

seek :: Line -> [StageEvent] -> [StageEvent]
seek "" = id
seek l = dropWhile (not . isLine l)

lineStage :: Line -> [StageEvent] -> [Character]
lineStage l = foldl onStage [] . takeWhile (not . isLine l)

lineSpeaker :: Line -> [StageEvent] -> Character
lineSpeaker l = head . mapMaybe speaker . seek l

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
