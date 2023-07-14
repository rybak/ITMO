{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.FromMonadFish where
import ITMOPrelude.Categories.MonadFish
import ITMOPrelude.Primitive

import ITMOPrelude.Categories hiding ((.))
import ITMOPrelude.Categories.MonadJoin

instance MonadFish m => Monad m where
    return = returnFish
    m >>= f = (>=>) id f m

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join = id >=> id

instance MonadFish m => Functor m where
    fmap f m = m >>= (return . f)
