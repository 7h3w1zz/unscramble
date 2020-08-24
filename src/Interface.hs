
module Interface
  ( getOptions
  , Input(..)
  , Unlimited(..)
  ) where

import Options.Applicative

mainArg = "CHARACTERS"
output = "WORDS"

data Input = Input
  { characters     :: String
  , unlimited      :: Maybe Unlimited
  }

data Unlimited = Unlimited
  { lengths     :: Either (Maybe Int, Maybe Int) (Maybe Int)
  , mustContain :: Maybe String
  }

getOptions :: IO (FilePath, Input)
getOptions = execParser optionParserInfo

optionParserInfo :: ParserInfo (FilePath, Input)
optionParserInfo = info (optionParser <**> helper)
  (  fullDesc
  <> header "unscramble - unscramble words on the command line"
  <> progDesc ("Unscrambles " ++ mainArg ++ " into a list of " ++ output)
  )

optionParser :: Parser (FilePath, Input)
optionParser = (,)
  <$> dictionaryFileOp
  <*> (Input <$> charactersOp <*> unlimitedOps)

unlimitedOps :: Parser (Maybe Unlimited)
unlimitedOps = pure Nothing <|>
  (unlimitedOp *> (Just <$> (Unlimited <$> lengthsOp <*> mustContainOp)))

unlimitedOp :: Parser ()
unlimitedOp = flag' ()
  (  long "unlimited"
  <> short 'u'
  <> help (output ++ " can contain any number " ++ mainArg)
  )

charactersOp :: Parser String
charactersOp = argument str
  (  metavar mainArg
  <> help "Characters to unscramble"
  )

-- **temporary**
dictionaryFileOp :: Parser FilePath
dictionaryFileOp = pure "/usr/share/dict/words"

lengthsOp :: Parser (Either (Maybe Int, Maybe Int) (Maybe Int))
lengthsOp = (Right <$> lengthOp) <|> (Left <$> ((,) <$> minLengthOp <*> maxLengthOp))

lengthOp :: Parser (Maybe Int)
lengthOp = option (Just <$> auto)
  (  long "length"
  <> short 'l'
  <> value Nothing
  <> metavar "<int>"
  <> help ("Exact length of " ++ output)
  )

minLengthOp :: Parser (Maybe Int)
minLengthOp = option (Just <$> auto)
  (  long "min-length"
  <> short 'm'
  <> value Nothing
  <> metavar "<int>"
  <> help ("Minimum length of " ++ output)
  )
maxLengthOp :: Parser (Maybe Int)
maxLengthOp = option (Just <$> auto)
  (  long "max-length"
  <> short 'M'
  <> value Nothing
  <> metavar "<int>"
  <> help ("Maximum length of " ++ output)
  )

mustContainOp :: Parser (Maybe String)
mustContainOp = option (Just <$> str)
  (  long "contains"
  <> short 'c'
  <> value Nothing
  <> metavar "<string>"
  <> help ("Characters that " ++ output ++ " must contain at least once")
  )
