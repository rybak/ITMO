{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show,Read,error)
import ITMOPrelude.List


data Tree a = Leaf | Node a (Tree a) (Tree a)


emptyTree :: Tree a
emptyTree = Leaf

insertRoot :: a -> Tree a -> Tree a
insertRoot a = Node a Leaf

insertLeft :: a -> Tree a -> Tree a
insertLeft a Leaf = Node a Leaf Leaf
insertLeft a (Node p left right) = Node p (insertLeft a left) right
 
insertRight :: a -> Tree a -> Tree a
insertRight a Leaf = Node a Leaf Leaf
insertRight a (Node p left right) = Node p left (insertRight a right)

turnLeft :: Tree a -> Tree a
turnLeft (Node n l (Node m c r)) = Node m (Node n l c) r

turnRight :: Tree a -> Tree a
turnRight (Node m (Node n l c) r) = (Node n l (Node m c r))

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Leaf = Leaf
treeMap f (Node a l r) = Node (f a) (treeMap f l) (treeMap f r)

treeFoldr :: (a -> b -> a -> a) -> a -> Tree b -> a
treeFoldr _ a Leaf = a
treeFoldr f a (Node b l r) = f (treeFoldr f a l) b (treeFoldr f a r)
