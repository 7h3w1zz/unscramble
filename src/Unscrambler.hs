module Unscrambler
  ( unscrambler
  ) where

import Data.List
import Data.Function
import Interface

-- Instances of Semigroup and Monoid for Bool allow
-- use of (instance Monoid m => Monoid (a -> m)).
-- This allows combining a -> Bool check functions
-- easily with (<>)
instance Semigroup Bool where
  (<>) = (&&)

instance Monoid Bool where
  mempty = True

-- If the Maybe value is Nothing, return mempty, otherwise
-- apply the function to the Just value.
-- Allows us to easily unwrap the Maybe options into a check,
-- which does nothing if the Maybe value was Nothing.
maybeEmpty :: Monoid b => (a -> b) -> Maybe a -> b
maybeEmpty = maybe mempty

-- main function, return unscrambled list of words.
-- Check each word against isUnscrambled
unscrambler :: [String] -> Input -> [String]
unscrambler dictionary input = filter (isUnscrambled input) dictionary

-- Check the String to see if it is a valid unscrambling
-- for the Input.
-- Combine filterCharacter and filterUnlimited checkers.
-- just pass the unlimited field of Input to filterUnlimited
isUnscrambled :: Input -> String -> Bool
isUnscrambled = filterCharacters <> (filterUnlimited . unlimited)

-- Check based on the characters field of Input.
-- Requires access to entire Input becuase it behaves
-- differently if if unlimited options exist.
--
-- If -u *was not* passed, check that the String is
--   a permutation of the character field
-- If -u *was* passed, check that the String contains only
--   characters that are present in the character field.
--   As the number of each character in the input does not matter,
--   deduplicate the input for this second case.
filterCharacters :: Input -> String -> Bool
filterCharacters (Input cs u) =
  maybe (isPermutation cs) (const $ isMadeOf $ nub cs) u

-- Combine all of the unlimited checkers together.
-- Pass respective fields to each checker
filterUnlimited :: Maybe Unlimited -> String -> Bool
filterUnlimited = maybeEmpty $
  (filterLengths . lengths) <> (filterMustContain . mustContain)

-- Check if the String contains characters given in
-- mustContain field, iff characters were given.
filterMustContain :: Maybe String -> String -> Bool
filterMustContain = maybeEmpty (flip isMadeOf)

-- Check that the length of the String is either between the
-- pair of Ints or exactly equal to the single Int
filterLengths :: Either (Maybe Int, Maybe Int) (Maybe Int) -> String -> Bool
filterLengths (Left (min, max)) =
  ((maybeEmpty (<=) min) <> (maybeEmpty (>=) max)) . length
filterLengths (Right x)         = (maybeEmpty (==) x) . length

-- Check that the second argument does not contain any items
-- that are not in the first.
isMadeOf :: Eq a => [a] -> [a] -> Bool
isMadeOf xs = and . map (flip contains xs)

-- Check that the two lists contain exactly the same items.
isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation = (==) `on` sort

-- Check that the list contains the element
contains :: Eq a => a -> [a] -> Bool
contains x = or . map (== x)

