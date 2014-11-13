{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL empList fun) = GL (emp : empList) (fun + empFun emp)

instance Monoid GuestList where
  mempty                        = GL [] 0
  mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f i Node { rootLabel = r, subForest = [] } = f r i
treeFold f i Node { rootLabel = r, subForest = s  } = f r (foldl (treeFold f) i s)