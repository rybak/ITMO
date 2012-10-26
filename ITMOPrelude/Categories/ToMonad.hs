{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories

-- Из этих
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

-- делаем эту

instance MonadFish m => Monad m where
    return = returnFish
    m >>= f = (>=>) id f m

instance MonadJoin m => Monad m where
    return = returnJoin
    m >>= f = join $ fmap f m

