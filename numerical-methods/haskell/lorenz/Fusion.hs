{-# LANGUAGE ExistentialQuantification, BangPatterns #-}
module Fusion
    (
        Step,
        Stream,
        streamList,
        unstreamList,
        takeWhile1,
        mapList,
        iterateStream,
        tailStream,
        zipWithStream,
        lastList,
        length2,
        PairS,
        makePair,
        uncurry',
        snd',
        fst'
    ) where

data Step s a = Done | Skip !s | Yield !a !s

data Stream a = forall s.
    Stream (s -> Step s a) !s

data Switch = S1 | S2

infixl 2 :*:
data PairS a b = !a :*: !b

fst' (a :*: _) = a
snd' (_ :*: b) = b

uncurry' ::  (a -> b -> c) -> (PairS a b) -> c
uncurry' f (a :*: b) = f a b

makePair !a !b = a :*: b

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p = unstreamList . (takeWhile1Stream p) . streamList

takeWhile1Stream p (Stream next0 s0) = Stream next (s0 :*: S1)
    where
      {-# INLINE next #-}
      next !(s :*: S1) = case next0 s of
                Done    -> Done
                Skip s' -> Skip (s' :*: S1)
                Yield x s' | p x       -> Yield x (s' :*: S1)
                           | otherwise -> Yield x (s' :*: S2)
      next !(s :*: S2) = Done

mapList :: (a -> b) -> [a] -> [b]
mapList f = unstreamList . (mapStream f) . streamList

mapStream :: (a -> b) -> Stream a -> Stream b
mapStream f (Stream next0 s0) = Stream next s0
    where next !s = case next0 s of
                    Done       -> Done
                    Skip s'    -> Skip s'
                    Yield x s' -> Yield (f x) s'

iterateStream :: (a -> a) -> a -> Stream a
iterateStream f start = Stream next start
    where next !s = Yield (f s) (f s)
{-# INLINE [0] iterateStream #-}

streamList :: [a] -> Stream a
{-# INLINE [0] streamList #-}
streamList s  = Stream next s
    where next []       = Done
          next (x:xs)   = Yield x xs

unstreamList :: Stream a -> [a]
unstreamList (Stream next s0) = unfold s0
    where unfold !s = case next s of
                        Done       -> []
                        Skip s'    -> unfold s'
                        Yield x s' -> x : unfold s'
{-# INLINE [0] unstreamList #-}

{-# RULES "STREAM streamList/unstreamList fusion" forall s. streamList (unstreamList s) = s #-}

data C s = C0 !s
         | C1 !s

tailStream :: Stream a -> Stream a
tailStream (Stream next0 s0) = Stream next (C0 s0)
    where
      next (C0 s) = case next0 s of
                      Done       -> error "tailStream"
                      Skip s'    -> Skip (C0 s')
                      Yield _ s' -> Skip (C1 s')
      next (C1 s) = case next0 s of
                      Done       -> Done
                      Skip s'    -> Skip    (C1 s')
                      Yield x s' -> Yield x (C1 s')
{-# INLINE [0] tailStream #-}

data M a = N | J !a

zipWithStream :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithStream f (Stream next0 sa0) (Stream next1 sb0) =
    Stream next (sa0 :*: sb0 :*: N)
    where
      next (sa :*: sb :*: N) = case next0 sa of
                Done -> Done
                Skip sa' -> Skip (sa' :*: sb :*: N)
                Yield a sa' -> Skip (sa' :*: sb :*: J a)
      next (sa' :*: sb :*: J a) = case next1 sb of
                Done -> Done
                Skip sb' -> Skip (sa' :*: sb' :*: J a)
                Yield b sb' -> Yield (f a b) (sa' :*: sb' :*: N)
{-# INLINE[0] zipWithStream #-}

lastList :: [a] -> a
lastList = lastStream . streamList

lastStream :: Stream a -> a
lastStream (Stream next s0) = loop0_last s0
    where
      loop0_last !s = case next s of
          Done       -> error "lastStream"
          Skip s'    -> loop0_last  s'
          Yield x s' -> loop_last x s'
      {-# INLINE loop0_last #-}
      loop_last !x !s = case next s of
          Done        -> x
          Skip s'     -> loop_last x  s'
          Yield x' s' -> loop_last x' s'
      {-# INLINE loop_last #-}
{-# INLINE[0] lastStream #-}

length2 :: Integral b => [a] -> b
length2 = lengthI . streamList

lengthI :: Integral b => Stream a -> b
lengthI (Stream next s0) = loop_length 0 s0
    where
      loop_length !z s  = case next s of
          Done       -> z
          Skip    s' -> loop_length z s'
          Yield _ s' -> loop_length (z + 1) s'
{-# INLINE[0] lengthI #-}

