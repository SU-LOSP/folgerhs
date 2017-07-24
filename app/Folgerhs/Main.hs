module Main (main) where

import Options.Applicative
import Data.Monoid ((<>))

import Folgerhs.Stage
import Folgerhs.Protagonism (protagonism)
import Folgerhs.Presence (presence)
import Folgerhs.Conversations (conversations)

data Config = Presence FilePath Float
            | Protagonism FilePath
            | Conversations FilePath Int Bool Line

config :: Parser Config
config = hsubparser
       ( command "presence"
         ( info presenceConfig (progDesc "Line-by-line presence on stage as CSV") )
      <> command "conversations"
         ( info conversationsConfig (progDesc "Animated stage") )
      <> command "protagonism"
         ( info protagonismConfig (progDesc "Speaker ratio per character") )
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

protagonismConfig :: Parser Config
protagonismConfig = Protagonism
       <$> strArgument
            ( metavar "FILENAME"
           <> help "File to parse" )

conversationsConfig :: Parser Config
conversationsConfig = Conversations
       <$> strArgument
            ( metavar "FILENAME"
           <> help "File to parse" )
       <*> option auto
            ( long "rate"
            <> value 1
            <> metavar "RATE"
            <> help "Lines per second")
       <*> switch
            ( long "with-unnamed"
            <> help "Include unnamed characters too")
       <*> strOption
            ( long "seek-line"
            <> value ""
            <> metavar "ACT.SCENE.LINE"
            <> help "Start animation from given line")

execute :: Config -> IO ()
execute (Presence f r) = presence f r
execute (Protagonism f) = protagonism f
execute (Conversations f lps wu sl) = conversations f lps wu sl

main :: IO ()
main = execParser opts >>= execute
    where
        desc = "Toolset for Folger Shakespeare Library's XML annotated plays"
        opts = info (helper <*> config) ( progDesc desc <> fullDesc )
