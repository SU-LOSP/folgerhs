module Folgerhs.Conversations (conversations) where

import System.Exit

import Data.Maybe
import Data.Array
import Data.List ((\\), union, lookup)
import Data.Char (isLower)
import Data.Maybe (fromMaybe)

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game

import Folgerhs.Stage as S
import Folgerhs.Parse (parse)

type Palette = (Character -> Color)
data State = Paused | Resumed
type Play =  (State, (Array Int StageEvent), Int, Palette)

colors :: [Color]
colors = cycle [ red, green, blue, yellow, magenta, rose, violet, azure,
                 aquamarine, chartreuse, orange ]

selectColor :: [Character] -> Palette
selectColor chs ch = fromMaybe (greyN 0.5) $ lookup ch (zip chs colors)

newPlay :: [StageEvent] -> Play
newPlay ses = ( Paused
              , (listArray (1, length ses) ses)
              , 1
              , (selectColor $ characters ses) 
              )

boxW :: Float
boxW = 140

boxH :: Float
boxH = 40

speak :: Picture -> Picture
speak p = pictures [color (greyN 0.85) (rectangleSolid (boxW+10) (boxH+10)), p]

enter :: Picture -> Picture
enter p = pictures [p, color (withAlpha 0.5 black) (rectangleSolid boxW boxH)]

exit :: Picture -> Picture
exit = enter

charPic :: Character -> Color -> StageEvent -> Picture
charPic ch c se = let box = color c $ rectangleSolid boxW boxH
                      name = translate (-60) (-4) $ scale 0.1 0.1 $ text ch
                      pic = pictures [box, name]
                   in case se of
                        Entrance chs -> if elem ch chs
                                           then enter pic
                                           else pic
                        Exit chs -> if elem ch chs
                                       then exit pic
                                       else pic
                        Speech ch' -> if ch == ch'
                                         then speak pic
                                         else pic
                        _ -> pic

transArc :: Float -> Float -> Picture -> Picture
transArc d a p = let (x, y) = mulSV d $ unitVectorAtAngle a
                  in translate x y p

optSplitUp :: Float -> Int -> (Float, Float)
optSplitUp a i = let i' = fromIntegral i
                  in (max a (a*i' / (2*pi)), 2*pi / i')

clock :: Line -> Picture
clock = translate (-60) (-10) . scale 0.3 0.3 . color white . text

charPics :: [Character] -> (Character -> Color) -> StageEvent -> Picture
charPics chs cf se = let pics = map (\ch -> charPic ch (cf ch) se) chs
                         (d, a) = optSplitUp 200 (length chs)
                      in pictures [ transArc d (i*a) pic
                                  | (i, pic) <- zip [0..] pics ]

curLine :: Play -> Line
curLine (_, ses, i, _) = let past = [ ses ! i' | i' <- [fst (bounds ses) .. i] ]
                          in fromMaybe "0" $ listToMaybe $ reverse $ mapMaybe S.line past

playPic :: Play -> IO Picture
playPic p@(_, ses, i, cf) = let l = curLine p
                                chs = lineStage l (elems ses)
                             in return $ pictures [ charPics chs cf (ses ! i), clock l ]

playEvent :: Event -> Play -> IO Play
playEvent (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
playEvent (EventKey (SpecialKey KeySpace) Down _ _) (Paused, ses, i, cf) = return (Resumed, ses, i, cf)
playEvent (EventKey (SpecialKey KeySpace) Down _ _) (Resumed, ses, i, cf) = return (Paused, ses, i, cf)
playEvent _ p = return p

playStep :: Float -> Play -> IO Play
playStep _ (Resumed, ses, i, cf) = return (Resumed, ses, (i+1), cf)
playStep _ p = return p

replicateChanges :: [StageEvent] -> [StageEvent]
replicateChanges [] = []
replicateChanges (se:ses) = let r = replicateChanges ses
                             in case se of
                                  Entrance _ -> replicate 5 se ++ r
                                  Exit _ -> replicate 5 se ++ r
                                  _ -> se : r

hasName :: Character -> Bool
hasName = any isLower . takeWhile (/= '.')

conversations :: FilePath -> Int -> Bool -> Line -> IO ()
conversations f lps wu sl = let dis = FullScreen (1280, 800)
                                bg = greyN 0.05
                                scf = if wu then const True else hasName
                                np = newPlay . replicateChanges . selectCharacters scf . seek sl . parse
                             in do source <- readFile f
                                   playIO dis bg lps (np source) playPic playEvent playStep
