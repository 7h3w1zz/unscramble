{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module UnscrambleOpts
  ( getOptions
  ) where

import Options.Applicative
import Unscrambler (Input(..))
import Utility

getOptions :: IO (FilePath, Input)
getOptions = execParser optionParserInfo

mainArg = "CHARACTERS"
output = "WORDS"

optionParserInfo :: ParserInfo (FilePath, Input)
optionParserInfo = info (optionParser <**> helper)
  (  fullDesc
  <> progDesc ("Unscrambles " ++ mainArg ++ " into a list of " ++ output)
  <> header "unscramble - unscramble words on the command line"
  )

optionParser :: Parser (FilePath, Input)
optionParser = do
  characters <- charactersOp
  dictionaryFile <- dictionaryFileOp
  ~(minLength, maxLength) <- lengthsOp
  mustContain <- mustContainOp
  pure (dictionaryFile, Input{..})

charactersOp = argument str
  (  value []
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
  <> value 0
  <> showDefault
  <> metavar "<int>"
  <> help ("Exact length of " ++ output  ++ ", 0 for no limit")
  )

minLengthOp = option auto
  (  long "min-length"
  <> short 'm'
  <> value 0
  <> showDefault
  <> metavar "<int>"
  <> help ("Minimum length of " ++ output)
  )

maxLengthOp = option auto
  (  long "max-length"
  <> short 'M'
  <> value 0
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
