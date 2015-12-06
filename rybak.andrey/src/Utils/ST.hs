{-
Like the state monad, but the transformer version.  Allows us to combine IO actions 
into the state monad.
-}

module Utils.ST where

import Control.Monad.Trans

data ST s m a = ST {runST :: s -> m (a,s)}

instance Monad m => Monad (ST s m) where
	return a = ST $ \s -> return (a,s) -- the return from the m monad!!
	st >>= f = ST $ \s -> do
		(a1,s1) <- runST st s -- run the st on the initial state
		(a2,s2) <- runST (f a1) s1 -- apply f to a1 getting another st and apply it to s1
		return $ (a2,s2) -- push through m

instance MonadTrans (ST s) where
	lift m = ST $ \s -> do
		a <- m -- use an m monad action to obtain a
		return (a,s) -- return for the m monad

{- |
Often in the  state monad we do the following.  Get the entire state, which is some data type, and return 
and aspect of it.  Here we generalize this.
-}
getST :: Monad m => (s -> a) -> ST s m a
getST g = ST $ \s -> return (g s,s)

updateST :: Monad m => (s -> s) -> ST s m ()
updateST u = ST $ \s -> return ((),u s)

setST :: Monad m => s -> ST s m ()
setST s = ST $ \s' -> return ((),s)

-- | A simple example.  Gets a number from IO inside the state monad
testStIO :: ST Int IO Int
testStIO = do
	x' <- lift getLine 
	let x = read x' :: Int
	cou <- ST $ \s -> return (s,s)
	let z = x + cou
	return z

testIOMain :: IO (Int,Int)
testIOMain = runST testStIO 1

