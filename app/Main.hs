module Main where

main :: IO ()
main = getOptions >>= validate >>= display . unscramble
