module BinarySearchTree 
  ( Tree (..)
  , toList
  , fromList
  , createTree
  , size
  , insert
  , find
  , delete
  , traverseMax
  , prop_tree
  )
where

import Data.Maybe
import Test.QuickCheck

data Tree = Empty | Sub Int Tree Tree
  deriving (Show, Eq)

instance Arbitrary Tree where
  arbitrary = genTree

genTree :: Gen Tree
genTree = do
  numbers <- listOf $ elements [1..10]
  return $ fromList numbers

toList :: Tree -> [Int]
toList Empty = []
toList (Sub n l r) = toList l ++ n : toList r

fromList :: [Int] -> Tree
fromList xs = foldl insert Empty xs

createTree :: Int -> Tree
createTree n = Sub n Empty Empty

size :: Tree -> Int
size Empty = 0
size (Sub _ l r) = 1 + size l + size r

prop_size :: [Int] -> Bool
prop_size xs = (length xs) == (size $ fromList xs)

insert :: Tree -> Int -> Tree
insert Empty i = createTree i
insert (Sub n left right) i
  | n >= i    = (Sub n (insert left i) right)
  | otherwise = (Sub n left (insert right i))

find :: Tree -> Int -> Maybe Int
find Empty _ = Nothing
find (Sub n left right) i
  | n == i = Just i
  | n <  i = find right i
  | n >  i = find left i

delete :: Tree -> Int -> Tree
delete Empty _ = Empty
delete (Sub x left right) del
  | x == del = deleteAction (Sub x left right) del
  | x <  del = Sub x left (delete right del)
  | x >  del = Sub x (delete left del) right
  where deleteAction :: Tree -> Int -> Tree
        deleteAction (Sub n Empty Empty) i -- Leaf node? Simply delete.
          = Empty
        deleteAction (Sub n Empty (Sub c cl cr)) i -- One child node, replace parent with child.
          = Sub c cl cr
        deleteAction (Sub n (Sub c cl cr) Empty) i
          = Sub c cl cr
        deleteAction (Sub n (Sub c cl cr) rightSub) i -- Two children, find the inorder predecessor and replace the node with it.
          = let pre = fromJust $ traverseMax (Sub c cl cr) 
                leftSub = delete (Sub c cl cr) pre
            in Sub pre leftSub rightSub

traverseMax :: Tree -> Maybe Int
traverseMax Empty = Nothing
traverseMax (Sub n _ Empty) = Just n
traverseMax (Sub _ _ r) = traverseMax r

prop_tree :: Tree -> (Int -> Int -> Bool) -> (Int -> Int -> Bool) -> Bool
prop_tree Empty _ _ = True
prop_tree (Sub n l r) lop rop = subtreeSize l n lop && subtreeSize r n rop
  where subtreeSize :: Tree -> Int -> (Int -> Int -> Bool) -> Bool
        subtreeSize Empty _ _ = True
        subtreeSize (Sub i left right) top op =
              (top `op` i) && subtreeSize left top op && subtreeSize right top op && subtreeSize left i lop && subtreeSize right i rop

prop_bst :: Tree -> Bool
prop_bst t = prop_tree t (>=) (<)

prop_bst_ops :: [Int] -> Bool
prop_bst_ops xs = let tree = fromList xs in
    checkContents xs tree && checkContents (drop 1 xs) (delete tree (head xs))
  where checkContents l t = map (\n -> fromJust $ find t n) l == l

