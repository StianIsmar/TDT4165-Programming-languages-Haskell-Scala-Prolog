module Tree
    (Tree(..)
    ) where

-- TASK 3.2
-- Binary Trees

data Tree a = Branch (Tree a) a (Tree a) | Leaf a
  deriving (Eq, Show)

-- The Foldable instance might prove tricky to define, so
-- defining the specific functions first may be easier!
treeSum :: (Num a) => Tree a -> a
treeSum (Leaf x) = x
treeSum (Branch left v right) = v + treeSum (left) + treeSum (right)

treeConcat :: Tree String -> String
treeConcat (Leaf x) = x
treeConcat (Branch left v right) = treeConcat left ++ v ++ treeConcat right

treeMaximum :: (Ord a) => Tree a -> a
treeMaximum (Leaf x) = x
treeMaximum (Branch lefttree v righttree) = max (max (treeMaximum lefttree) v) (treeMaximum righttree)
  

-- Write a Foldable instance for Tree.
instance Foldable Tree where
    foldr op acc (Leaf x) = x `op` acc 
    foldr op acc (Branch lefttree v righttree) =  foldr op (v `op` foldr op acc righttree) lefttree
-- (v `f` foldr op acc righttree) returns the accumulator! Remember that!


