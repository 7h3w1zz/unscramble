{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module UnscrambleOpts
  ( getOptions
  , Options(..)
  ) where

import Options.Applicative

data Options = Options
  { characters     :: String
  , dictionaryFile :: FilePath
  , minLength      :: Int
  , maxLength      :: Int
  , mustContain       :: String
  }

getOptions :: IO Options
getOptions = execParser optionParserInfo

mainArg = "LETTERS"

optionParserInfo :: ParserInfo Options
optionParserInfo = info (optionParser <**> helper)
  (  fullDesc
  <> progDesc ("Unscramble " ++ mainArg)
  <> header "unscramble - unscramble words on the command line"
  )

optionParser :: Parser Options
optionParser = do
  characters <- argument str
    (  value []
    <> showDefaultWith (const "All characters")
    <> metavar mainArg
    <> help "Characters to unscramble"
    )
  -- **temporary**
  dictionaryFile <- pure "/usr/share/dict/words"
  minLength <- option auto
    (  long "min-length"
    <> short 'm'
    <> value 0
    <> showDefault
    <> metavar "INT"
    <> help "Minimum length of unscrambled words"
    )
  maxLength <- option auto
    (  long "max-length"
    <> short 'M'
    <> value 0
    <> showDefault
    <> metavar "INT"
    <> help "Maximum length of unscrambled words, 0 for no limit"
    )
  mustContain <- option str
    (  long "mustContain"
    <> short 'c'
    <> value []
    <> showDefaultWith (const "<None>")
    <> metavar "string"
    <> help "Characters that words must contain at least once"
    )
  pure Options{..}
