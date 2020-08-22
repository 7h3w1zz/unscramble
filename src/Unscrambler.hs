{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Unscrambler
  ( unscrambler
  , Input(..)
  ) where

data Input = Input
  { characters  :: String
  , minLength   :: Int
  , maxLength   :: Int
  , mustContain :: String
  }

unscrambler :: [String] -> Input -> [String]
unscrambler dictionary input = filter (isUnscrambled input) dictionary

isUnscrambled :: Input -> String -> Bool
isUnscrambled Input{..} = mempty
  <> emptyIfTrue (minLength == 0) ((>= minLength) . length)
  <> emptyIfTrue (maxLength == 0) ((<= maxLength) . length)
  <> emptyIfTrue (characters == []) (isMadeOf characters)
  <> emptyIfTrue (mustContain == []) (flip containsOneOf mustContain)

emptyIfTrue :: Monoid m => Bool -> m -> m
emptyIfTrue b m = if b then mempty else m

isMadeOf :: Eq a => [a] -> [a] -> Bool
isMadeOf xs = and . map (flip contains xs)

containsOneOf :: Eq a => [a] -> [a] -> Bool
containsOneOf xs = or . map (flip contains xs)

contains :: Eq a => a -> [a] -> Bool
contains x = or . map (== x)

instance Semigroup Bool where
  (<>) = (&&)

instance Monoid Bool where
  mempty = True
