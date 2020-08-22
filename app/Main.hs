module Main where

import UnscrambleOpts

main :: IO ()
main = getOptions >>= validate >>= display . unscramble


-- **Work In Progress**
validate _ = return True
display = putStrLn
unscramble b = if b then "success!" else "failure :("
