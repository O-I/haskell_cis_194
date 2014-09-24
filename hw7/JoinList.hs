module JoinList where

import Data.Monoid
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
l1 +++ l2 = Append (tag l1 <> tag l2) l1 l2

-- Exercise 2

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize = getSize . size . tag

-- Exercise 2.1

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ n jl@(Append m l1 l2)
  | n < 0         = Nothing
  | n > jlSize jl = Nothing
  | n < jlSize l1 = indexJ n l1
  | otherwise     = indexJ (n - jlSize l1) l2

-- Exercise 2.2

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl@(Append m l1 l2)
  | n <= 0                    = jl
  | n >= (getSize . size $ m) = Empty
  | n >= jlSize l1            = dropJ (n - jlSize l1) l2
  | otherwise                 = (dropJ n l1) +++ l2
dropJ _ (Single _ _) = Empty

-- Exercise 2.3

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl@(Append m l1 l2)
  | n <= 0                    = Empty
  | n >= (getSize . size $ m) = jl
  | n >= jlSize l1            = l1 +++ (takeJ (n - jlSize l1) l2)
  | otherwise                 = takeJ (jlSize l1) l1
takeJ _ s@(Single _ _) = s

-- tests Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s