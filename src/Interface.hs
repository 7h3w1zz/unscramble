{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Interface
  ( getOptions
  ) where

import Options.Applicative
import Unscrambler
import Utility

getOptions :: IO (FilePath, Input)
getOptions = execParser optionParserInfo

mainArg = "CHARACTERS"
output = "WORDS"

optionParserInfo :: ParserInfo (FilePath, Input)
optionParserInfo = info (optionParser <**> helper)
  (  fullDesc
  <> header "unscramble - unscramble words on the command line"
  <> progDesc ("Unscrambles " ++ mainArg ++ " into a list of " ++ output)
  )

optionParser :: Parser (FilePath, Input)
optionParser = do
  characters <- charactersOp
  dictionaryFile <- dictionaryFileOp
  ~(minLength, maxLength) <- lengthsOp
  mustContain <- mustContainOp
  pure (dictionaryFile, Input{..})

charactersOp = argument str
  (  value (characters inputDefault)
  <> showDefaultWith (const "All characters")
  <> metavar mainArg
  <> help "Characters to unscramble"
  )

-- **temporary**
dictionaryFileOp = pure "/usr/share/dict/words"

lengthsOp = (dup <$> lengthOp) <|> ((,) <$> minLengthOp <*> maxLengthOp)

lengthOp = option auto
  (  long "length"
  <> short 'l'
  <> value (minLength inputDefault)
  <> showDefault
  <> metavar "<int>"
  <> help ("Exact length of " ++ output  ++ ", 0 for no limit")
  )

minLengthOp = option auto
  (  long "min-length"
  <> short 'm'
  <> value (minLength inputDefault)
  <> showDefault
  <> metavar "<int>"
  <> help ("Minimum length of " ++ output)
  )

maxLengthOp = option auto
  (  long "max-length"
  <> short 'M'
  <> value (maxLength inputDefault)
  <> showDefault
  <> metavar "<int>"
  <> help ("Maximum length of " ++ output ++ ", 0 for no limit")
  )

mustContainOp = option str
  (  long "contains"
  <> short 'c'
  <> value []
  <> showDefaultWith (const "<None>")
  <> metavar "<string>"
  <> help ("Characters that " ++ output ++ " must contain at least once")
  )
