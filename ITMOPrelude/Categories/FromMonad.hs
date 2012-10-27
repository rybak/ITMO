{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories.FromMonad where
import ITMOPrelude.Categories

import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

instance Monad m => MonadFish m where
    returnFish = return
    f >=> g = \a -> (return a) >>= f >>= g

instance Monad m => MonadJoin m where
    returnJoin = return
    join = flip (>>=) id

