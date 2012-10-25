{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories.ToMonadFish where
import ITMOPrelude.Categories.MonadFish
import ITMOPrelude.Primitive
-- Из этих
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadJoin

-- делаем эти
instance Monad m => MonadFish m where
    returnFish = return
    f >=> g = \a -> (return a) >>= f >>= g

instance MonadJoin m => MonadFish m where
    returnFish = returnJoin
    f >=> g = \a -> join $ fmap g $ f a

