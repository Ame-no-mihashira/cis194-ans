module Towers where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n < 1 = []
  | otherwise =
      hanoi m a c b
        ++ (a, b)
        : hanoi m c b a
 where
  m = n - 1

hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' n a b c d
  | n < 3 = hanoi n a b c
  | otherwise =
      hanoi' (n - k) a c b d
        ++ hanoi k a b d
        ++ hanoi' (n - k) c b a d
 where
  k =
    let
      n' = fromIntegral n
     in
      (flip (-) 1)
        . round
        . sqrt
        $ 2 * n' + 1
