module Fibonacci where

fib :: Integer -> Integer
fib n
  | n < 2 = n
  | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (drop 1 fibs2)

data Stream a
  = Stream a (Stream a)

instance (Show a) => Show (Stream a) where
  show = show . take 16 . streamToList

streamToList :: Stream a -> [a]
streamToList (Stream y ys) =
  y : streamToList ys

streamRepeat :: a -> Stream a
streamRepeat y =
  Stream y $ streamRepeat y

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream y ys) =
  Stream (f y) $ streamMap f ys

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f y =
  Stream y
    . streamFromSeed f
    $ f y

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Stream y ys) zs =
  Stream y $
    streamInterleave zs ys

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler =
  streamInterleave
    (streamRepeat 0)
    (streamMap (+ 1) ruler)

ruler' :: [Integer]
ruler' =
  merge
    (repeat 0)
    (map (+ 1) ruler')
 where
  merge [] ys = ys
  merge (x : xs) ys = x : merge ys xs

x :: Stream Integer
x =
  Stream 0
    . Stream 1
    $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Stream n $ streamRepeat 0
  negate = streamMap negate
  (+) (Stream y ys) (Stream z zs) =
    Stream (y + z) $ ys + zs
  (*) (Stream y ys) rhs@(Stream z zs) =
    Stream (y * z) $
      streamMap (* y) zs + ys * rhs
  abs = streamMap abs
  signum = streamMap signum

instance Fractional (Stream Integer) where
  (/) (Stream y ys) (Stream z zs) = q
   where
    q =
      Stream (div y z)
        . streamMap (flip div z)
        $ ys - q * zs

fibs3 :: Stream Integer
fibs3 = (x /) $ 1 - x - x ^ 2

data Matrix
  = Matrix Integer Integer Integer Integer
  deriving (Show)

instance Num Matrix where
  fromInteger n =
    Matrix n 0 0 n
  (+) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
    Matrix
      (a11 + b11)
      (a12 + b12)
      (a21 + b21)
      (a22 + b22)
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
    Matrix
      (a11 * b11 + a12 * b21)
      (a11 * b12 + a12 * b22)
      (a21 * b11 + a22 * b21)
      (a21 * b12 + a22 * b22)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = a
 where
  f = Matrix 1 1 1 0
  (Matrix _ _ _ a) = f ^ n
