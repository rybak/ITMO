{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.FromMonadJoin where
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Primitive

import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadFish

instance MonadJoin m => Monad m where
    return = returnJoin
    m >>= f = join $ fmap f m

instance MonadJoin m => MonadFish m where
    returnFish = returnJoin
    f >=> g = \a -> join $ fmap g $ f a

