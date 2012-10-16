module ITMOPrelude.Algebra where

import Prelude (Show,Read,show)
import ITMOPrelude.Primitive

class Monoid a where
    mneutral :: a
    moperation :: a -> a -> a

instance Monoid Unit where
    mneutral = Unit
    moperation _ _ = Unit

instance Monoid Nat where
    mneutral = Zero
    moperation = (+.)

instance Monoid Int where
    mneutral = Pos $ Succ Zero
    moperation = (.*.)

instance Monoid Rat where
    mneutral = Rat (Pos $ Zero) (Succ Zero)
    moperation = (%+)

class Group a where
    gneutral :: a
    gneg :: a -> a
    goperation :: a -> a -> a

instance Group Unit where
    gneutral = Unit
    gneg _ = Unit
    goperation _ _ = Unit

instance Group Int where
    gneutral = intZero
    gneg = intNeg
    goperation = (.+.)

instance Group Rat where
    gneutral = Rat (Pos $ Succ Zero) (Succ Zero)
    gneg = ratInv
    goperation = (%*)
