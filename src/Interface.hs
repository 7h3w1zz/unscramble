module Interface
  ( getOptions
  , Input(..)
  , Unlimited(..)
  ) where

import Options.Applicative

-- Name variables
mainArg = "CHARACTERS"
output = "WORDS"

-- Type for all the options
data Input = Input
  { characters     :: String
  , unlimited      :: Maybe Unlimited
  }

-- More options, only used when --unlimited is passed
data Unlimited = Unlimited
  { lengths     :: Either (Maybe Int, Maybe Int) (Maybe Int)
  , mustContain :: Maybe String
  }

-- Main function
getOptions :: IO (FilePath, Input)
getOptions = execParser optionParserInfo

-- Add --help option to get final Parser with helper,
-- then add top-level info with info
optionParserInfo :: ParserInfo (FilePath, Input)
optionParserInfo = info (optionParser <**> helper)
  (  fullDesc
  <> header "unscramble - unscramble words on the command line"
  <> progDesc ("Unscrambles " ++ mainArg ++ " into a list of " ++ output)
  )

-- Combine all the Parsers together
optionParser :: Parser (FilePath, Input)
optionParser = (,)
  <$> dictionaryFileOp
  <*> (Input <$> charactersOp <*> unlimitedOps)

-- Combine all options behind --unlimited flag.
-- Discard unlimitedOp's output, but sequence it first.
-- If --unlimited isn't present, entire parser fails and
-- (pure Nothing) is returned
unlimitedOps :: Parser (Maybe Unlimited)
unlimitedOps =
  (unlimitedOp *> (Just <$> (Unlimited <$> lengthsOp <*> mustContainOp)))
  <|> pure Nothing

-- --unlimited doesn't return anything useful,
-- it is just a gate for the rest of the options behind it
unlimitedOp :: Parser ()
unlimitedOp = flag' ()
  (  long "unlimited"
  <> short 'u'
  <> help (output ++ " can contain any number of " ++ mainArg)
  )

-- Main argument
-- Went back and forth on whether it should be required
-- or not, eventually decided it should.  To change, just
-- have it return a Maybe instead (and change Unscramble.hs to match)
charactersOp :: Parser String
charactersOp = argument str
  (  metavar mainArg
  <> help "Characters to unscramble"
  )

-- Get the dictionary file, defaults to "/usr/share/dict/words"
dictionaryFileOp :: Parser FilePath
dictionaryFileOp = option str
  (  long "dictionary-file"
  <> short 'd'
  <> value "/usr/share/dict/words"
  <> showDefault
  <> metavar "<file>"
  <> help "Whitespace-separated word list to solve from"
  )

-- Combine the lengths options.  Having both exact length and
-- min/max together doesn't make sense, so either return
-- single int for -l or pair of ints for -m and/or -M.
-- Sequenced so -l appears first in help text
lengthsOp :: Parser (Either (Maybe Int, Maybe Int) (Maybe Int))
lengthsOp = (Right <$> lengthOp) <|> (Left <$> ((,) <$> minLengthOp <*> maxLengthOp))

-- Get exact length.  Return Nothing iff option not provided.
lengthOp :: Parser (Maybe Int)
lengthOp = option (Just <$> auto)
  (  long "length"
  <> short 'l'
  <> value Nothing
  <> metavar "<int>"
  <> help ("Exact length of " ++ output)
  )

-- Get minimum length of output.
-- Return Nothing iff option not provided
minLengthOp :: Parser (Maybe Int)
minLengthOp = option (Just <$> auto)
  (  long "min-length"
  <> short 'm'
  <> value Nothing
  <> metavar "<int>"
  <> help ("Minimum length of " ++ output)
  )

-- Get maximum length of output.
-- Return Nothing iff option not provided
maxLengthOp :: Parser (Maybe Int)
maxLengthOp = option (Just <$> auto)
  (  long "max-length"
  <> short 'M'
  <> value Nothing
  <> metavar "<int>"
  <> help ("Maximum length of " ++ output)
  )

-- Get list of characters output must contain.
-- Return Nothing iff option not provided
mustContainOp :: Parser (Maybe String)
mustContainOp = option (Just <$> str)
  (  long "contains"
  <> short 'c'
  <> value Nothing
  <> metavar "<string>"
  <> help ("Characters that " ++ output ++ " must contain at least once")
  )
