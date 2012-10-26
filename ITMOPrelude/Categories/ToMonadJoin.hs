{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories.ToMonadJoin where
import ITMOPrelude.Primitive
import ITMOPrelude.Categories.MonadJoin

-- Из этих
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadFish

-- делаем эту

instance Monad m => MonadJoin m where
    returnJoin = return
    join = flip (>>=) id

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join = id >=> id
