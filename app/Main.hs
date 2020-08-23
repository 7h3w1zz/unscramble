module Main where

import Interface
import Unscrambler

main :: IO ()
main = getOptions >>= validate >>= (display . (uncurry unscrambler))

validate :: (FilePath, Input) -> IO ([String], Input)
validate (path, input) = (,) <$> getDictionary path <*> pure input

getDictionary :: FilePath -> IO [String]
getDictionary = fmap words . readFile

display :: [String] -> IO ()
display = sequence_ . (map putStrLn)
