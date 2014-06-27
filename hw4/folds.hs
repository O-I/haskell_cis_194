-- Exercise 2  Folding with trees

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert item Leaf = Node 0 Leaf item Leaf
insert item (Node i l m r)
  | lh < rh   = Node i li m r
  | lh > rh   = Node i l m ri
  | otherwise = Node (h + 1) l m ri
  where lh = height l
        rh = height r
        li = insert item l
        ri = insert item r
        h  = height ri

height :: Tree a -> Integer
height Leaf           = -1
height (Node i l m r) = i

-- Exercise 3  More folds!

-- xor using foldr
xor :: [Bool] -> Bool
xor = foldr defXor False

defXor :: Bool -> Bool -> Bool
defXor p q = (p || q) && not (p && q)

-- xor using filter
xor' :: [Bool] -> Bool
xor' = odd . length . (filter (== True))