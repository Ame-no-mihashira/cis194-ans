module Golf where

import Data.List (transpose)
import Text.Read (Lexeme (String))

skips :: [a] -> [[a]]
skips =
  zipWith skip [1 ..]
    . (\xs -> replicate (length xs) xs)
 where
  skip n =
    map snd
      . filter ((== 0) . (flip mod n) . fst)
      . zip [1 ..]

localMaxima :: [Integer] -> [Integer]
localMaxima =
  map (\(_, y, _) -> y)
    . filter (\(x, y, z) -> 2 * y > x + z)
    . windows
 where
  windows (x : y : zs@(z : _)) = (x, y, z) : windows zs
  windows _ = []

histogram :: [Integer] -> String
histogram =
  unlines
    . reverse
    . transpose
    . (\xs -> map (display (maximum $ map snd xs)) xs)
    . zip digits
    . flip map digits
    . flip count
 where
  digits = [0 .. 9]
  count n =
    length
      . filter (== n)
  display m (x, n) =
    show x
      ++ '='
      : replicate n '*'
      ++ replicate (m - n) ' '
