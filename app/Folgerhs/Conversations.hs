module Folgerhs.Conversations (conversations) where

import System.Exit

import Data.Maybe
import Data.Array
import Data.List ((\\), union, lookup)
import Data.Char (isLower)
import Data.Maybe (fromMaybe)

import Graphics.Gloss as G
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game

import Folgerhs.Stage as S
import Folgerhs.Parse (parse)

type Palette = (Character -> Color)
data State = Paused | Resumed
    deriving (Eq, Show)
type Play =  (State, (Array Int StageEvent), Int, Palette)

takeA :: (Ix i, Eq e) => Int -> (Array i e) -> [e]
takeA i a = take i $ elems a

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

arrow :: Picture
arrow = color white $ pictures [ G.line [o, t1]
                               , G.line [o, t2]
                               , G.line [o, t3]
                               ]
                                   where o = (0, boxH/4)
                                         t1 = mulSV (boxH/(-4)) (0,1)
                                         a = mulSV (boxH/6) (0,1)
                                         t2 = rotateV (pi/4) a
                                         t3 = rotateV (-(pi/4)) a

above :: Picture -> Picture
above = translate 0 (boxH*3/4)

enter :: Picture -> Picture
enter p = pictures [p, color (withAlpha 0.8 black) (rectangleSolid boxW boxH), above (rotate 180 arrow)]

exit :: Picture -> Picture
exit p = pictures [p, color (withAlpha 0.8 black) (rectangleSolid boxW boxH), above arrow]

charPic :: Character -> Bool -> Color -> Picture
charPic ch sp c = let box = color c $ rectangleSolid boxW boxH
                      name = translate (-60) (-4) $ scale 0.1 0.1 $ text ch
                      pic = pictures [box, name]
                   in if sp then speak pic else pic

transArc :: Float -> Float -> Picture -> Picture
transArc d a p = let (x, y) = mulSV d $ unitVectorAtAngle a
                  in translate x y p

optPos :: Float -> Int -> (Float, Float)
optPos a i = let i' = fromIntegral i
             in (max a (a*i' / (2*pi)), 2*pi / i')

curLine :: Play -> Line
curLine (_, ses, i, _) = let past = [ ses ! i' | i' <- [fst (bounds ses) .. i] ]
                          in fromMaybe "0" $ listToMaybe $ reverse $ mapMaybe maybeLine past

clock :: Play -> Picture
clock = translate (-60) (-10) . scale 0.3 0.3 . color white . text . curLine

charPics :: Play -> [Picture]
charPics p@(_,ses,i,cf) = let sp = accumSpeaker (takeA i ses)
                              chs = accumStage (takeA i ses)
                              charPic' ch = charPic ch (ch == sp) (cf ch)
                           in case ses ! i of
                                (Entrance chs') -> map charPic' (chs \\ chs') ++ map (enter . charPic') chs'
                                (Exit chs') -> map charPic' (chs \\ chs') ++ map (exit . charPic') chs'
                                _ -> map charPic' chs

playPic :: Play -> IO Picture
playPic p = let pics = charPics p
                (d, a) = optPos 200 (length pics)
                posPics = [ transArc d (i*a) pic | (i, pic) <- zip [0..] pics ]
             in return $ pictures $ clock p : posPics

playEvent :: Event -> Play -> IO Play
playEvent (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
playEvent (EventKey (SpecialKey KeySpace) Down _ _) (Paused, ses, i, cf) = return (Resumed, ses, i, cf)
playEvent (EventKey (SpecialKey KeySpace) Down _ _) (Resumed, ses, i, cf) = return (Paused, ses, i, cf)
playEvent (EventKey (SpecialKey KeyLeft) Down _ _) (p, ses, i, cf) = return (p, ses, max (fst $ bounds ses) (i-10), cf)
playEvent (EventKey (SpecialKey KeyRight) Down _ _) (p, ses, i, cf) = return (p, ses, min (snd $ bounds ses) (i+10), cf)
playEvent _ p = return p

playStep :: Float -> Play -> IO Play
playStep _ (Resumed, ses, i, cf) = return (Resumed, ses, (i+1), cf)
playStep _ p = return p

hasName :: Character -> Bool
hasName = any isLower . takeWhile (/= '.')

replicateChanges :: Int -> [StageEvent] -> [StageEvent]
replicateChanges i [] = []
replicateChanges i (se:ses) = let r = replicateChanges i ses
                               in case se of
                                    Entrance _ -> replicate i se ++ r
                                    Exit _ -> replicate i se ++ r
                                    _ -> se : r

conversations :: FilePath -> Int -> Bool -> Line -> IO ()
conversations f lps wu sl = let dis = FullScreen (1280, 800)
                                bg = greyN 0.05
                                scf = if wu then const True else hasName
                                np = newPlay . replicateChanges 10 . selectCharacters scf . seek sl . parse
                             in do source <- readFile f
                                   playIO dis bg lps (np source) playPic playEvent playStep
