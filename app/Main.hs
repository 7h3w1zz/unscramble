module Main where

import Control.Exception
import System.Exit
import Interface
import Unscrambler

main :: IO ()
main = getOptions >>= validate >>= (display . (uncurry unscrambler))

validate :: (FilePath, Input) -> IO ([String], Input)
validate (path, input) = (,) <$> getDictionary path <*> pure input

getDictionary :: FilePath -> IO [String]
getDictionary = fmap words . myReadFile

myReadFile :: FilePath -> IO String
myReadFile fileName = readFile fileName `onException`
  (putStrLn ("Could not read file: " ++ fileName) >> exitFailure)

display :: [String] -> IO ()
display = sequence_ . (map putStrLn)
