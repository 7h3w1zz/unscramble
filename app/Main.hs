{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative

main :: IO ()
main = getOptions >>= validate >>= display . unscramble

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

data Options = Options
  { letters        :: String
  , dictionaryFile :: FilePath
  }

-- **Work In Progress**
validate _ = return True
display = putStrLn
unscramble b = if b then "success!" else "failure :("
