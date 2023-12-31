module ITMOPrelude.Categories where

import Prelude (Show,Read,show,flip)
import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree

class Category g where
    id :: g a a
    (.) :: g b c -> g a b -> g a c

instance Category (->) where
    id a = a
    (.) = (ITMOPrelude.Primitive..)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor (Pair a) where
    fmap f (Pair x y) = Pair x $ f y

instance Functor (Either a) where
    fmap f (Left a) = Left a
    fmap f (Right a) = Right (f a)

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

instance Monad (Either a) where
    return a = Right a
    (Left a) >>= f = Left a
    (Right a) >>= f = f a

--------------------------------------------------------------------------------
-- Монада State

newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
    -- return :: a -> (State s) a)
    return a = State $ \s -> (s, a)
    -- >>= :: (State s) a -> (a -> (State s) b) -> (State s) b
    act >>= f = State $ \s -> let (s', a) = runState act s in runState (f a) s'
