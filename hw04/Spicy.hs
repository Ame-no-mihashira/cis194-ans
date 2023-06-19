module Spicy where

import Data.List
import Data.Set (fromList, toList)

fun1' :: [Integer] -> Integer
fun1' =
  product
    . map (flip (-) 2)
    . filter even

fun2' :: Integer -> Integer
fun2' =
  sum
    . filter even
    . takeWhile (> 1)
    . iterate hailgen
 where
  hailgen n
    | odd n = 3 * n + 1
    | otherwise = div n 2

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree xs =
  Node height (foldTree lhs) x (foldTree rhs)
 where
  len = length xs
  height = floor . logBase 2 $ fromIntegral len
  half = div len 2
  (lhs, x : rhs) = splitAt half xs

xor :: [Bool] -> Bool
xor =
  odd
    . foldl' (+) 0
    . map (\x -> if x then 2 else 0)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  map (\x -> 2 * x + 1)
    . (\\) [1 .. n]
    . filter (<= n)
    -- deduplicate
    . toList
    . fromList
    -- starting set
    . concat
    $ [ [ i + j + 2 * i * j
        | i <- [1 .. j]
        ]
      | j <- [1 .. n]
      ]
