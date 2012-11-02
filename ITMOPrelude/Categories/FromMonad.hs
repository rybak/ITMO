{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.FromMonad where
import ITMOPrelude.Categories hiding ((.))
import Prelude (flip, (.) )

import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

instance Monad m => MonadFish m where
    returnFish = return
    f >=> g = \a -> (return a) >>= f >>= g

instance Monad m => MonadJoin m where
    returnJoin = return
    join = flip (>>=) id

instance Monad m => Functor m where
    fmap f m = m >>= (return . f)
