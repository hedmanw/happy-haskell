module SplayTree where

import Data.Maybe
import Test.QuickCheck
import BinarySearchTree

splay :: Tree -> Int -> Tree
splay Empty _ = Empty
splay (Sub p l r) i
  | p == i = (Sub p l r)
splay (Sub p (Sub x a b) c) i -- zig left
  | x == i = Sub x a (Sub p b c)
splay (Sub p a (Sub x b c)) i -- zig right
  | x == i = Sub x (Sub p a b) c
splay (Sub g (Sub p (Sub x a b) c) d) i -- zig-zig left
  | x == i = Sub x a (Sub p b (Sub g c d))
splay (Sub g (Sub p a (Sub x b c)) d) i -- zig-zag left
  | x == i = Sub x (Sub p a b) (Sub g c d)
splay (Sub g a (Sub p b (Sub x c d))) i -- zig-zig right
  | x == i = Sub x (Sub p (Sub g a b) c) d
splay (Sub g a (Sub p (Sub x b c) d)) i -- zig-zag right
  | x == i = Sub x (Sub g a b) (Sub p c d)
splay (Sub g l r) i
  | g < i = Sub g l (splay r i)
  | g > i = Sub g (splay l i) r

splayInsert :: Tree -> Int -> Tree
splayInsert = undefined

splayFind :: Tree -> Int -> (Tree, Maybe Int)
splayFind = undefined

splayDelete :: Tree -> Int -> Tree
splayDelete = undefined

prop_contents :: [Int] -> Bool
prop_contents xs = let tree = fromList xs in
    map (\n -> fromJust $ find tree n) xs == xs

prop_content_after_splay :: [Int] -> Bool
prop_content_after_splay xs = let tree = fromList xs in
  all (\x -> prop_splay $ splay tree x) xs

prop_splay :: Tree -> Bool
prop_splay t = prop_tree t (>=) (<=)

