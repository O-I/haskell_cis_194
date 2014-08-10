{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import VarExprT
import qualified StackVM
import qualified Data.Map as M

-- Exercise 1

eval :: ExprT -> Integer
eval (ExprT.Lit x)       = x
eval (ExprT.Add (x) (y)) = (eval x) + (eval y)
eval (ExprT.Mul (x) (y)) = (eval x) * (eval y)

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr expr = case parse of
                 Nothing -> Nothing
                 Just x  -> Just $ eval x
  where
    parse = parseExp ExprT.Lit ExprT.Add ExprT.Mul expr

-- Exercise 3

class Expr a where
  add :: a -> a -> a
  mul :: a -> a -> a
  lit :: Integer -> a

instance Expr ExprT where
  add = ExprT.Add
  mul = ExprT.Mul
  lit = ExprT.Lit

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

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y
  lit                       = MinMax

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7
  lit x                 = Mod7 $ x `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7
testStackVM = testExp :: Maybe StackVM.Program

-- Exercise 5

instance Expr StackVM.Program where
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]
  lit x   = [StackVM.PushI x]

compile :: String -> Maybe StackVM.Program
compile str = parseExp lit add mul str

-- Exercise 6

class HasVars a where
  var :: String -> a

instance Expr VarExprT where
  add = VarExprT.Add
  mul = VarExprT.Mul
  lit = VarExprT.Lit

instance HasVars VarExprT where
  var s = Var s