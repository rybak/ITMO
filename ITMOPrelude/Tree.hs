{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show,Read,error)
import ITMOPrelude.List


data Tree a = Leaf | Node a (Tree a) (Tree a)


emptyTree :: Tree a
emptyTree = Leaf

insertRoot :: a -> Tree a -> Tree a
insertRoot a = Node a Leaf

addLeft :: a -> Tree a -> Tree a
addLeft a Leaf = Node a Leaf Leaf
addLeft a (Node p left right) = Node p (addLeft a left) right

addRight :: a -> Tree a -> Tree a
addRight a Leaf = Node a Leaf Leaf
addRight a (Node p left right) = Node p left (addRight a right)
