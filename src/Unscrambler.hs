module Unscrambler
  ( unscrambler
  ) where

import Data.List
import Data.Function
import Interface

unscrambler :: [String] -> Input -> [String]
unscrambler dictionary input = filter (isUnscrambled input) dictionary

isUnscrambled :: Input -> String -> Bool
isUnscrambled = filterCharacters <> (filterUnlimited . unlimited)

filterCharacters :: Input -> String -> Bool
filterCharacters (Input cs u) =
  maybe (isPermutation cs) (const $ isMadeOf $ nub cs) u

filterUnlimited :: Maybe Unlimited -> String -> Bool
filterUnlimited = maybeEmpty $
  (filterLengths . lengths) <> (filterMustContain . mustContain)

filterMustContain :: Maybe String -> String -> Bool
filterMustContain = maybeEmpty (flip isMadeOf)

filterLengths :: Either (Maybe Int, Maybe Int) (Maybe Int) -> String -> Bool
filterLengths (Left (min, max)) = ((maybeEmpty (<=) min) <> (maybeEmpty (>=) max)) . length
filterLengths (Right x)         = (maybeEmpty (==) x) . length

isMadeOf :: Eq a => [a] -> [a] -> Bool
isMadeOf xs = and . map (flip contains xs)

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation = (==) `on` sort

contains :: Eq a => a -> [a] -> Bool
contains x = or . map (== x)

maybeEmpty :: Monoid b => (a -> b) -> Maybe a -> b
maybeEmpty = maybe mempty

instance Semigroup Bool where
  (<>) = (&&)

instance Monoid Bool where
  mempty = True
