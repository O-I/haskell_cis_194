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