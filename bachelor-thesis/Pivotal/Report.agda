module Report where

data Zero : Set where

record One : Set where
  constructor ⟨⟩

data Two : Set where
  tt ff : Two

So : Two → Set
So tt = One
So ff = Zero

_⇒_ : Set → Set → Set
P ⇒ T = {{p : P}} → T
infixr 3 _⇒_

not : Two → Two
not tt = ff
not ff = tt

if_then_else_ : ∀ {X} b → (So b ⇒ X) → (So (not b) ⇒ X) → X
if_then_else_ tt t _ = t
if_then_else_ ff _ f = f
infix 1 if_then_else_

data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ
  
_ℕ≤_ _ℕ≥_ : ℕ → ℕ → Two
zero ℕ≤ y = tt
succ x ℕ≤ zero = ff
succ x ℕ≤ succ y = x ℕ≤ y

x ℕ≥ y = y ℕ≤ x
-- zero ℕ> y = ff
-- succ x ℕ> zero = tt
-- succ x ℕ> succ y = x ℕ> y

min max : ℕ → ℕ → ℕ
min x y = if x ℕ≤ y then x else y
max x y = if x ℕ≥ y then x else y

{-# BUILTIN NATURAL ℕ #-}
{-# BUILTIN ZERO zero #-}
{-# BUILTIN SUC succ #-}

-- function composition

module Level where
  postulate Level : Set
  postulate lzero : Level
  postulate lsucc : Level → Level
  postulate _⊔_   : Level → Level → Level
  infixl 5 _⊔_
  {-# BUILTIN LEVEL     Level #-}
  {-# BUILTIN LEVELZERO lzero #-}
  {-# BUILTIN LEVELSUC  lsucc #-}
  {-# BUILTIN LEVELMAX  _⊔_   #-}
open Level

module MLTT where

  record Σ {a b} (A : Set a) (B : A → Set b) : Set (a ⊔ b) where
    constructor _,_
    field fst : A ; snd : B fst

  open Σ public
  _×_ : ∀ {a b} (A : Set a) → (B : Set b) → Set (a ⊔ b)
  A × B = Σ A (λ _ → B)
  infixr 5 _×_ _,_

open MLTT public

data expanded (P : Set) : Set where
  ⊤ : expanded P
  # : P → expanded P
  ⊥ : expanded P

expandRel : ∀ {P} → (P → P → Two) → (expanded P → expanded P → Two)
expandRel le _ ⊤ = tt
expandRel le ⊥ _ = tt
expandRel le (# x) (# y) = le x y
expandRel _ _ _ = ff

Rel : Set → Set1
Rel P = P × P → Set

module TryHeap2 where
  postulate
    P : Set
    _≤_ : P → P → Two

  _e≤_ : (expanded P) → (expanded P) → Two
  _e≤_ = expandRel _≤_
  data Heap : ℕ → (expanded P) → Set where
    hleaf : Heap zero ⊥
    hnode : ∀ {ra rb a b}
      → (p : P) → Heap ra a → Heap rb b
      → So (rb ℕ≤ ra) ⇒ So (a e≤ # p) ⇒ So (b e≤ # p)
      ⇒ Heap (succ rb) (# p)

  makeT : ∀ {ra rb x y} → (p : P) → Heap ra x → Heap rb y → So (x e≤ # p) ⇒ So (y e≤ # p) ⇒ Heap (succ (min rb ra)) (# p)
  makeT {ra} {rb} {x} {y} p a b = if rb ℕ≤ ra
    then (λ {{x₁}} → {! hnode ? a b!})
    else {!!}
