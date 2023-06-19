module Calc where

import ExprT
import Parser
import StackVM

import Data.Map qualified as M

eval :: ExprT -> Integer
eval expr = case expr of
  ExprT.Lit x -> x
  ExprT.Add lhs rhs -> eval lhs + eval rhs
  ExprT.Mul lhs rhs -> eval lhs * eval rhs

evalStr :: String -> Maybe Integer
evalStr s = do
  x <- parseExp ExprT.Lit ExprT.Add ExprT.Mul s
  return $ eval x

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>= 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit = Mod7 . flip mod 7
  add (Mod7 x) (Mod7 y) = Mod7 (flip mod 7 $ x + y)
  mul (Mod7 x) (Mod7 y) = Mod7 (flip mod 7 $ x * y)

testExp :: (Expr a) => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr StackVM.Program where
  lit x = [StackVM.PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExprT
  = Lit Integer
  | Add VarExprT VarExprT
  | Mul VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Calc.Lit
  add = Calc.Add
  mul = Calc.Mul

instance HasVars VarExprT where
  var = Calc.Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

-- see carefully how syntactic sugar makes this valid
instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = (\_ -> Just x)
  add f g m = do
    x <- f m
    y <- g m
    return $ x + y
  mul f g m = do
    x <- f m
    y <- g m
    return $ x * y

withVars ::
  [(String, Integer)] ->
  (M.Map String Integer -> Maybe Integer) ->
  Maybe Integer
withVars vs exp = exp $ M.fromList vs
