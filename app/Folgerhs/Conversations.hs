module Folgerhs.Conversations (conversations) where

import Data.List (lookup, intercalate, intersect)
import Data.Sequence (fromList)
import Data.Char (isLower)
import Data.Maybe (fromMaybe)

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import Folgerhs.Stage as S
import Folgerhs.Parse (parse)
import Folgerhs.Display (displayCharacter)


hasName :: Character -> Bool
hasName = any isLower . takeWhile (/= '.') . displayCharacter

selectCharacters :: (Character -> Bool) -> [Stage] -> [Stage]
selectCharacters f = map (\(l, s, cs) -> (l, s, filter f cs))

charPic :: Character -> Color -> Picture
charPic ch c = pictures [ color c $ rectangleSolid 120 50
                        , translate (-50) 0 $ scale 0.1 0.1 $ text $ displayCharacter ch ]

transCircum :: Float -> Float -> Picture -> Picture
transCircum d a p = let (x, y) = mulSV d $ unitVectorAtAngle a
                     in translate x y p

-- TODO Optimal radius

charPics :: [Character] -> (Character -> Color) -> Picture
charPics chs cf = let a = 2*pi / (fromIntegral $ length chs)
                   in pictures $
                       [ transCircum 300 (i*a) $ charPic ch (cf ch)
                       | (i, ch) <- zip [0..] chs ]

stagePic :: Stage -> (Character -> Color) -> Picture
stagePic (l, s, chs) cf = pictures $
    [ charPics chs cf
    , translate (-70) 0 $ scale 0.3 0.3 $ color white $ text $ l ]

colors :: [Color]
colors = [ red, green, blue, yellow, cyan, magenta, rose, violet, azure,
           aquamarine, chartreuse, orange ]

selectColor :: [Character] -> Character -> Color
selectColor chs ch = fromMaybe (greyN 0.5) $ lookup ch (zip chs colors)

stageAnim :: [Stage] -> Float -> Picture
stageAnim ss t = stagePic (ss !! (floor $ t*4)) (selectColor $ characters ss)

conversations :: FilePath -> IO ()
conversations f = do source <- readFile f
                     let ss = selectCharacters hasName $ perLine $ parse source
                     animate (FullScreen (1280, 800)) (greyN 0.05) (stageAnim ss)
                     return ()
