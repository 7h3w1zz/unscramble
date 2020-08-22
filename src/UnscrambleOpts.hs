{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module UnscrambleOpts
  ( getOptions
  , Options(..)
  ) where

import Options.Applicative

data Options = Options
  { letters        :: String
  , dictionaryFile :: FilePath
  }

getOptions :: IO Options
getOptions = execParser optionParserInfo

optionParserInfo :: ParserInfo Options
optionParserInfo = info (optionParser <**> helper)
  (  fullDesc
  <> progDesc "Unscramble LETTERS"
  <> header "unscramble - unscramble words on the command line"
  )

optionParser :: Parser Options
optionParser = do
  letters <- argument str
    (  metavar  "LETTERS"
    <> help  "Letters to unscramble"
    )
  -- **temporary**
  dictionaryFile <- pure "./words.txt"
  pure Options{..}
