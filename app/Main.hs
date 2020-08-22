module Main where

import qualified UnscrambleOpts as O

main :: IO ()
main = O.getOptions >>= validate >>= display . unscramble

validate :: O.Options -> IO Input
validate opts = Input (O.letters opts) <$> getDictionary (O.dictionaryFile opts)

getDictionary :: FilePath -> IO [String]
getDictionary = fmap words . readFile

data Input = Input
  { letters    :: String
  , dictionary :: [String]
  }

-- **Work In Progress**
display = putStrLn
unscramble _ = "success"
