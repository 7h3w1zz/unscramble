module Main where

import Control.Exception
import System.Exit
import Interface
import Unscrambler

main :: IO ()
main = getOptions >>= validate >>= (display . (uncurry unscrambler))

-- read file and parse into words
validate :: (FilePath, Input) -> IO ([String], Input)
validate (path, input) = (,) <$> getDictionary path <*> pure input

-- File is assumed to contain whitespace separated words
getDictionary :: FilePath -> IO [String]
getDictionary = fmap words . myReadFile

-- Wraps readFile to output nice string and IMMEDIATELY exits
-- on and exception
myReadFile :: FilePath -> IO String
myReadFile fileName = readFile fileName `onException`
  (putStrLn ("Could not read file: " ++ fileName) >> exitFailure)

-- prints output
display :: [String] -> IO ()
display = sequence_ . (map putStrLn)
