module ITMOPrelude.Categories where

import Prelude (Show,Read,show)
import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree

class Category g where
    id :: g a a
    composition :: g b c -> g a b -> g a c

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor (Pair a) where
    fmap f (Pair x y) = Pair x $ f y

instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just a) = Just (f a)

instance Functor List where
    fmap = map

instance Functor Tree where
    fmap = treeMap

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where
    return a = Just a
    (Nothing) >>= f = Nothing
    (Just a)  >>= f = f a

instance Monad List where
    return a = Cons a Nil
    xs >>= f = concat $ map f xs


