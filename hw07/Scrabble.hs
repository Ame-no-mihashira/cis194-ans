module Scrabble where

import Data.Char (toLower)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score c
  | elem c "aeilnorstu" = Score 1
  | elem c "dg" = Score 2
  | elem c "bcmp" = Score 3
  | elem c "fhvwy" = Score 4
  | elem c "k" = Score 5
  | elem c "jx" = Score 8
  | elem c "qz" = Score 10
  | otherwise = Score 0

scoreString :: String -> Score
scoreString = sum . map (score . toLower)

getScore (Score a) = a
