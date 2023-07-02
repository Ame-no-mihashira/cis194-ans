module Party where

import Data.Tree
import Employee

-- EX01

glCons :: Employee -> GuestList -> GuestList
glCons x@Emp{empFun = add} (GL xs cur) = GL (x : xs) (cur + add)

instance Semigroup GuestList where
  (GL xs a) <> (GL ys b) = GL (xs ++ ys) (a + b)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- EX02

-- improved by adapting Data.Tree.foldTree
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f Node{rootLabel = x, subForest = xss} =
  f x $ map (treeFold f) xss

-- EX03

nextLevel ::
  Employee ->
  [(GuestList, GuestList)] ->
  (GuestList, GuestList)
nextLevel emp xs = (lhs, rhs)
 where
  most = foldr moreFun mempty
  lhs = most $ map (glCons emp . snd) xs
  rhs = most $ map fst xs

-- EX04

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- EX05

fmtGL :: GuestList -> String
fmtGL (GL xs k) =
  "Total fun: "
    ++ show k
    ++ "\n"
    ++ concatMap fmtEmp xs
 where
  fmtEmp = (++ "\n") . empName

main :: IO ()
main = do
  desc <- readFile "company.txt"
  putStr . fmtGL . maxFun . read $ desc
