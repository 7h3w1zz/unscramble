module Main where

import UnscrambleOpts
import Validate

main :: IO ()
main = getOptions >>= validate >>= display . unscramble

-- **Work In Progress**
display = putStrLn
unscramble _ = "success"
