module Calc where

import ExprT
import Parser

-- Exercise 1

eval :: ExprT -> Integer
eval (Lit x)       = x
eval (Add (x) (y)) = (eval x) + (eval y)
eval (Mul (x) (y)) = (eval x) * (eval y)

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr expr = case parse of
                 Nothing -> Nothing
                 Just x  -> Just $ eval x
  where
    parse = parseExp Lit Add Mul expr

-- Exercise 3

class Expr a where
  add :: a -> a -> a
  mul :: a -> a -> a
  lit :: Integer -> a

instance Expr ExprT where
  add = Add
  mul = Mul
  lit = Lit

reify :: ExprT -> ExprT
reify = id

-- Exercise 4

instance Expr Integer where
  add = (+)
  mul = (*)
  lit = id

instance Expr Bool where
  add = (||)
  mul = (&&)
  lit = (> 0)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool