module Validate (validate) where

import qualified UnscrambleOpts as O
import qualified Unscrambler as U

validate :: O.Options -> IO ([String], U.Input)
validate opts =
  (,) <$> getDictionary (O.dictionaryFile opts)
      <*> pure U.Input
          { U.characters = (O.characters opts)
          , U.minLength = (O.minLength opts)
          , U.maxLength = (O.maxLength opts)
          , U.mustContain = (O.mustContain opts)
          }

getDictionary :: FilePath -> IO [String]
getDictionary = fmap words . readFile
