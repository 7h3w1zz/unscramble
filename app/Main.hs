module Main where

import UnscrambleOpts
import Validate
import Unscrambler

main :: IO ()
main = getOptions >>= validate >>= (display . (uncurry unscrambler))

display :: [String] -> IO ()
display = sequence_ . (map putStrLn)
