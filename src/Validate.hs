module Validate (validate) where

import qualified UnscrambleOpts as O

validate :: O.Options -> IO Input
validate opts = Input (O.letters opts) <$> getDictionary (O.dictionaryFile opts)

getDictionary :: FilePath -> IO [String]
getDictionary = fmap words . readFile

data Input = Input
  { letters    :: String
  , dictionary :: [String]
  }
