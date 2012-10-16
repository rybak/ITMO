module ITMOPrelude.Algebra where

import Prelude (Show,Read,show)
import ITMOPrelude.Primitive
import ITMOPrelude.List

class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

instance Monoid Unit where
    mempty = Unit
    mappend _ _ = Unit

instance Monoid Tri where
    mempty = EQ
    EQ `mappend` a = a
    LT `mappend` _ = LT
    GT `mappend` _ = GT

instance Monoid Nat where
    mempty = Zero
    mappend = (+.)

instance Monoid Int where
    mempty = Pos $ Zero
    mappend = (.+.)

instance Monoid Rat where
    mempty = Rat (Pos $ Succ Zero) (Succ Zero)
    mappend = (%*)

instance Monoid (List a) where
    mempty = Nil
    mappend = (++)

instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` a = a
    a `mappend` Nothing = a
    (Just a) `mappend` (Just b) = Just $ a `mappend` b

class Monoid a => Group a where
    gneg :: a -> a

instance Group Unit where
    gneg _ = Unit

instance Group Int where
    gneg = intNeg

instance Group Rat where
    gneg = ratInv
