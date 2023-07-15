module Utils.SM where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

data SM s a = SM (s -> (a,s))

runState :: (SM s a) -> (s -> (a,s))
runState (SM f) = f

instance Functor (SM s) where
    fmap = liftM
instance Applicative (SM s) where
    pure a = SM (\s -> (a,s))
    (<*>) = ap

instance Monad (SM s) where
    return = pure {- redundant in GHC 7.10, keep for lower versions of GHC -}
    {-
    Run f on the initial state.  You get a newState and a value.  Feed the value to g.
    Getting a newStateTransformer.  Update this system with the new state from f.
    -}
    (SM f) >>= g = SM (\initState ->
        let
            (a,newState) = f initState
            newST = g a 
        in
            (runState newST) newState
            )
