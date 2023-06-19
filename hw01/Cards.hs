module Cards where

toDigits :: Integer -> [Integer]
toDigits n
  | n < 10 = [n]
  | otherwise = (mod n 10) : toDigits (div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x : y : xs) = x : (2 * y) : doubleEveryOther xs
doubleEveryOther xs = xs

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' =
  zipWith (*) factors
 where
  factors = cycle [1, 2]

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate =
  (\x -> mod x 10 == 0)
    . sumDigits
    . doubleEveryOther'
    . toDigits
