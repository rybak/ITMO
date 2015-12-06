module Utils.SM where

data SM s a = SM (s -> (a,s))

runState :: (SM s a) -> (s -> (a,s))
runState (SM f) = f

instance Monad (SM s) where
	return a = SM (\s -> (a,s))
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
