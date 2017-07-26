module Main (main) where

import Options.Applicative
import Data.Monoid ((<>))

import Folgerhs.Stage
import Folgerhs.Speakers (speakers)
import Folgerhs.Presence (presence)
import Folgerhs.Animate (animation)

data Config = Presence FilePath Float
            | Speakers FilePath
            | Animate FilePath Int Bool Line

config :: Parser Config
config = hsubparser
       ( command "presence"
         ( info presenceConfig (progDesc "Line-by-line on stage presence as CSV") )
      <> command "animate"
         ( info animateConfig (progDesc "Animated character interaction") )
      <> command "speakers"
         ( info speakersConfig (progDesc "Speech ratio per character") )
       )

presenceConfig :: Parser Config
presenceConfig = Presence
       <$> strArgument
            ( metavar "FILENAME"
           <> help "File to parse" )
       <*> option auto
            ( long "relevance"
           <> value 0
           <> metavar "RATIO"
           <> help "Minimum appearance ratio" )

speakersConfig :: Parser Config
speakersConfig = Speakers
       <$> strArgument
            ( metavar "FILENAME"
           <> help "File to parse" )

animateConfig :: Parser Config
animateConfig = Animate
       <$> strArgument
            ( metavar "FILENAME"
           <> help "File to parse" )
       <*> option auto
            ( long "rate"
            <> value 10
            <> metavar "RATE"
            <> help "Lines per second")
       <*> switch
            ( long "without-unnamed"
            <> help "Exclude unnamed characters")
       <*> strOption
            ( long "seek-line"
            <> value "0"
            <> metavar "ACT.SCENE.LINE"
            <> help "Start animation from given line")

execute :: Config -> IO ()
execute (Presence f r) = presence f r
execute (Speakers f) = speakers f
execute (Animate f lps wu sl) = animation f lps wu sl

main :: IO ()
main = execParser opts >>= execute
    where
        desc = "Example usage of the toolset for Folger Shakespeare Library's TEI-encoded plays"
        opts = info (helper <*> config) ( progDesc desc <> fullDesc )
