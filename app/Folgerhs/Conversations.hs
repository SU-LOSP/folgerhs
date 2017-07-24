module Folgerhs.Conversations (conversations) where

import Data.Array
import Data.List (lookup)
import Data.Char (isLower)
import Data.Maybe (fromMaybe)

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import Folgerhs.Stage as S
import Folgerhs.Parse (parse)
import Folgerhs.Display (displayCharacter)


type Palette = Character -> Color

colors :: [Color]
colors = [ red, green, blue, yellow, cyan, magenta, rose, violet, azure,
           aquamarine, chartreuse, orange ]

selectColor :: [Character] -> Palette
selectColor chs ch = fromMaybe (greyN 0.5) $ lookup ch (zip chs colors)

charPic :: Character -> Color -> Picture
charPic ch c = pictures [ color c $ rectangleSolid 120 50
                        , translate (-50) 0 $ scale 0.1 0.1 $ text $ displayCharacter ch ]

transArc :: Float -> Float -> Picture -> Picture
transArc d a p = let (x, y) = mulSV d $ unitVectorAtAngle a
                  in translate x y p

optSplitUp :: Float -> Int -> (Float, Float)
optSplitUp a i = let i' = fromIntegral i
                  in (max a (a*i' / (2*pi)), 2*pi / i')

charPics :: [Character] -> Palette -> Picture
charPics chs cf = let (d, a) = optSplitUp 250 (length chs)
                   in pictures $
                       [ transArc d (i*a) $ charPic ch (cf ch)
                       | (i, ch) <- zip [0..] chs ]

stagePic :: Stage -> Palette -> Picture
stagePic (l, s, chs) cf = pictures $
    [ charPics chs cf
    , translate (-60) (-10) $ scale 0.3 0.3 $ color white $ text $ l ]

hasName :: Character -> Bool
hasName = any isLower . takeWhile (/= '.') . displayCharacter

selectCharacters :: (Character -> Bool) -> [Stage] -> [Stage]
selectCharacters f = map (\(l, s, cs) -> (l, s, filter f cs))

stageAnim :: Float -> Array Int Stage -> Float -> Picture
stageAnim lps ssArray t = let frame = floor (t * lps)
                              palette = selectColor (characters $ elems ssArray)
                           in if inRange (bounds ssArray) frame
                                 then stagePic (ssArray ! frame) palette
                                 else blank

conversations :: FilePath -> Float -> IO ()
conversations f lps = do source <- readFile f
                         let ss = selectCharacters hasName $ perLine $ parse source
                         let ssArray = listArray (1, length ss) ss
                         animate (FullScreen (1280, 800)) (greyN 0.05) (stageAnim lps ssArray)
                         return ()
