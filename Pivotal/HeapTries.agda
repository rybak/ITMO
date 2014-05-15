module HeapTries where

data ⊥ : Set where

record ⊤ : Set where
  constructor ⟨⟩

-- function composition

module Level where
  postulate Level : Set
  postulate lzero : Level
  postulate lsucc : Level → Level
  postulate _⊔_   : Level → Level → Level
  infixl 6 _⊔_
  {-# BUILTIN LEVEL     Level #-}
  {-# BUILTIN LEVELZERO lzero #-}
  {-# BUILTIN LEVELSUC  lsucc #-}
  {-# BUILTIN LEVELMAX  _⊔_   #-}
open Level

module Function where
  _∘_ : ∀ {α β γ}
      → {A : Set α} {B : A → Set β} {C : {x : A} → B x → Set γ}
      → (f : {x : A} → (y : B x) → C y)
      → (g : (x : A) → B x)
      → ((x : A) → C (g x))
  f ∘ g = λ x → f (g x)
  
  -- Simple composition
  _∘′_ : ∀ {α β γ}
      → {A : Set α} {B : Set β} {C : Set γ}
      → (B → C) → (A → B) → (A → C)
  f ∘′ g = f ∘ g
  
  -- Flip
  flip : ∀ {α β γ}
       → {A : Set α} {B : Set β} {C : A → B → Set γ} 
       → ((x : A) → (y : B) → C x y)
       → ((y : B) → (x : A) → C x y)
  flip f x y = f y x
  
  -- Identity
  id : ∀ {α} {A : Set α} → A → A
  id x = x
  
  -- Constant function
  const : ∀ {α β}
       → {A : Set α} {B : Set β}
       → (A → B → A)
  const x y = x

open Function public

module Logic where

  ⊥-elim : ∀ {a} {A : Set a} → ⊥ → A
  ⊥-elim ()

  ¬ : ∀ {a} → Set a → Set a
  ¬ P = P → ⊥

  private
   module DummyAB {α β} {A : Set α} {B : Set β} where
    contradiction : A → ¬ A → B
    contradiction a ¬a = ⊥-elim (¬a a)

    contraposition : (A → B) → (¬ B → ¬ A)
    contraposition = flip _∘′_

    contraposition¬ : (A → ¬ B) → (B → ¬ A)
    contraposition¬ = flip
    
    →¬² : ∀ {α} {A : Set α} → A → ¬ (¬ A)
    →¬² a = λ z → z a

  open DummyAB public

open Logic public

module MLTT where
  infix 4 _≡_
  data _≡_ {a} {A : Set a} (x : A) : A → Set a where
    refl : x ≡ x
  {-# BUILTIN EQUALITY _≡_ #-}
  {-# BUILTIN REFL    refl #-}

  record Σ {a b} (A : Set a) (B : A → Set b) : Set (a ⊔ b) where
    constructor _,_
    field fst : A ; snd : B fst

  open Σ public
  _×_ : ∀ {a b} (A : Set a) → (B : Set b) → Set (a ⊔ b)
  A × B = Σ A (λ _ → B)
  infixr 5 _×_ _,_
  module ≡-Prop where
   private
    module DummyA {α} {A : Set α} where
      -- _≡_ is symmetric
      sym : {x y : A} → x ≡ y → y ≡ x
      sym refl = refl
  
      -- _≡_ is transitive
      trans : {x y z : A} → x ≡ y → y ≡ z → x ≡ z
      trans refl refl = refl
  
      -- _≡_ is substitutive
      subst : ∀ {γ} {P : A → Set γ} {x y} → x ≡ y → P x → P y
      subst refl p = p
  
    private
     module DummyAB {α β} {A : Set α} {B : Set β} where
      -- _≡_ is congruent
      cong : ∀ (f : A → B) {x y} → x ≡ y → f x ≡ f y
      cong f refl = refl

      subst₂ : ∀ {ℓ} {P : A → B → Set ℓ} {x y u v} → x ≡ y → u ≡ v → P x u → P y v
      subst₂ refl refl p = p
  
    private
     module DummyABC {α β γ} {A : Set α} {B : Set β} {C : Set γ} where
      cong₂ : ∀ (f : A → B → C) {x y u v} → x ≡ y → u ≡ v → f x u ≡ f y v
      cong₂ f refl refl = refl
  
    open DummyA public
    open DummyAB public
    open DummyABC public
  open ≡-Prop public

open MLTT public

module Decidable where
  data Dec {a} (A : Set a) : Set a where
    yes : ( a :   A) → Dec A
    no  : (¬a : ¬ A) → Dec A
open Decidable public

Rel₂ : Set → Set₁
Rel₂ A = A → A → Set
 
data Tri {A : Set} (_<_ _==_ _>_ : Rel₂ A) (a b : A) : Set where
  tri< :   (a < b) → ¬ (a == b) → ¬ (a > b) → Tri _<_ _==_ _>_ a b
  tri= : ¬ (a < b) →   (a == b) → ¬ (a > b) → Tri _<_ _==_ _>_ a b
  tri> : ¬ (a < b) → ¬ (a == b) →   (a > b) → Tri _<_ _==_ _>_ a b
 
flip₁ : ∀ {A B : Set} {C : Set₁} → (A → B → C) → B → A → C
flip₁ f a b = f b a

Cmp : {A : Set} → Rel₂ A → Rel₂ A → Set
Cmp {A} _<_ _==_ = {x y : A} → Tri (_<_) (_==_) (flip₁ _<_) x y

data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

infixl 6 _ℕ+_
_ℕ+_ : ℕ → ℕ → ℕ
zero ℕ+ b = b
succ a ℕ+ b = succ (a ℕ+ b)

data _ℕ≤_ : Rel₂ ℕ where
  z≤n : ∀ {n} → zero ℕ≤ n
  s≤s : ∀ {n m} → n ℕ≤ m → succ n ℕ≤ succ m

_ℕ<_ _ℕ≥_ _ℕ>_ : Rel₂ ℕ
n ℕ< m = succ n ℕ≤ m
n ℕ> m = m ℕ< n
n ℕ≥ m = m ℕ≤ n

{-# BUILTIN NATURAL ℕ #-}
{-# BUILTIN ZERO zero #-}
{-# BUILTIN SUC succ #-}

lemma-succ-≡ : ∀ {n} {m} → succ n ≡ succ m → n ≡ m
lemma-succ-≡ refl = refl
lemma-succ-≤ : ∀ {n} {m} → succ (succ n) ℕ≤ succ m → succ n ℕ≤ m
lemma-succ-≤ (s≤s r) = r

cmpℕ : {x y : ℕ} → Tri _ℕ<_ _≡_ _ℕ>_ x y
cmpℕ {zero} {zero} = tri= (λ ()) refl (λ ())
cmpℕ {zero} {succ y} = tri< (s≤s z≤n) (λ ()) (λ ())
cmpℕ {succ x} {zero} = tri> (λ ()) (λ ()) (s≤s z≤n)
cmpℕ {succ x} {succ y} with cmpℕ {x} {y}
... | tri<  a ¬b ¬c = tri< (s≤s a) (contraposition lemma-succ-≡ ¬b) (contraposition lemma-succ-≤ ¬c)
... | tri> ¬a ¬b  c = tri> (contraposition lemma-succ-≤ ¬a) (contraposition lemma-succ-≡ ¬b) (s≤s c)
... | tri= ¬a  b ¬c = tri= (contraposition lemma-succ-≤ ¬a) (cong succ b) (contraposition lemma-succ-≤ ¬c)

less : {A : Set} {_<_ _==_ : Rel₂ A} → Cmp _<_ _==_ → (a b : A) → Dec (a < b)
less cmp a b with cmp {a} {b}
less cmp a b | tri< y y' y0 = yes y
less cmp a b | tri= y y' y0 = no y
less cmp a b | tri> y y' y0 = no y

eq : {A : Set} {_<_ _==_ : Rel₂ A} → Cmp _<_ _==_ → (a b : A) → Dec (a == b)
eq cmp a b with cmp {a} {b}
eq cmp a b | tri< y y' y0 = no y'
eq cmp a b | tri= y y' y0 = yes y'
eq cmp a b | tri> y y' y0 = no y' 

more : {A : Set} {_<_ _==_ : Rel₂ A} → Cmp _<_ _==_ → (a b : A) → Dec (b < a)
more cmp a b with cmp {a} {b}
more cmp a b | tri< y y' y0 = no y0
more cmp a b | tri= y y' y0 = no y0
more cmp a b | tri> y y' y0 = yes y0

Trans : {A : Set} → Rel₂ A → Set
Trans {A} _rel_ = {a b c : A} → (a rel b) → (b rel c) → (a rel c)

Sym : {A : Set} → Rel₂ A → Set
Sym {A} _rel_ = {a b : A} → (a rel b) → (b rel a)

data OR (A B : Set) : Set where
  orA : A → OR A B
  orB : B → OR A B

data AND (A B : Set) : Set where
  and : A → B → AND A B

lemma-m1 : ∀ {A} {B} → ¬ (OR A B) → AND (¬ A) (¬ B)
lemma-m1 x = and (λ x₁ → x (orA x₁)) (λ x₁ → x (orB x₁))
lemma-m2 : ∀ {A} {B} → AND (¬ A) (¬ B) → ¬ (OR A B)
lemma-m2 (and a b) (orA x) = a x
lemma-m2 (and a b) (orB x) = b x

less-eq : {A : Set} {_<_ _==_ : Rel₂ A} → Cmp _<_ _==_ → (a b : A) → Dec (OR (a < b) (a == b))
less-eq cmp a b with cmp {a} {b}
less-eq cmp a b | tri< x x₁ x₂ = yes (orA x)
less-eq cmp a b | tri= x x₁ x₂ = yes (orB x₁)
less-eq cmp a b | tri> x x₁ x₂ = no (lemma-m2 (and x x₁))

lemma-not< : ∀ {a b : ℕ} → ¬ (a ℕ< b) → OR (a ≡ b) (b ℕ< a)
lemma-not< {a} {b} ab with cmpℕ {a} {b}
lemma-not< ab | tri< x x₁ x₂ = contradiction x ab
lemma-not< ab | tri= x x₁ x₂ = orA x₁
lemma-not< ab | tri> x x₁ x₂ = orB x₂

min max : ℕ → ℕ → ℕ
min x y with cmpℕ {x} {y}
... | tri< _ _ _ = x
... | _ = y
max x y with cmpℕ {x} {y}
... | tri> _ _ _  = x
... | _ = y

module TryHeap (A : Set) (_<_ _==_ : Rel₂ A) (cmp : Cmp _<_ _==_) where

  data expanded (A : Set) : Set where
    # : A → expanded A
    top : expanded A
  _<E_ _=E_ : Rel₂ (expanded A)
  # x <E # y = x < y
  x <E top = ⊤
  top <E x = ⊥

  # x =E # y = x == y
  top =E top = ⊤
  _ =E _ = ⊥

  leq : (a b : expanded A) → Set
  leq a b = Dec (OR (a <E b) (a =E b))

  heapgood : (a b : expanded A) → (p : A) → Set
  heapgood a b p = (leq (# p) a) × (leq (# p) b)

  data Tree : Set where
    leaf : Tree
    fork : Tree → Tree → Tree

  data Path : Tree → Set where
    end : Path leaf
    here : ∀ {l r} → Path (fork l r)
    left : ∀ {l r} → Path l → Path (fork l r)
    right : ∀ {l r} → Path r → Path (fork l r)

  data Full : ℕ → Tree → Set where
    none : Full zero leaf
    fork : ∀ {n l r} → Full n l → Full n r
      → Full (succ n) (fork l r)

  data Filled : ℕ → Tree → Set where
    empty : Filled zero leaf
    left-filled : ∀ {n l r} → Filled (succ n) l → Full n r
      → Filled (succ (succ n)) (fork l r)
    left-full : ∀ {n l r} → Full n l → Filled n r
      → Filled (succ n) (fork l r)
    
  data HeapState : Set where
    full almost : HeapState

  data Heap : (expanded A) → (h : ℕ) → HeapState → Set where
    eh : Heap top zero full
    ns : ∀ {n} {x y} → (p : A)
        → heapgood x y p
        → (a : Heap x (succ n) full)
        → (b : Heap y n full)
        → Heap (# p) (succ (succ n)) almost
    nf : ∀ {n} {x y} → (p : A)
        → heapgood x y p
        → (a : Heap x n full)
        → (b : Heap y n full)
        → Heap (# p) (succ n) full
    nl : ∀ {n} {x y} → (p : A)
        → heapgood x y p
        → (a : Heap x (succ n) almost)
        → (b : Heap y n full)
        → Heap (# p) (succ (succ n)) almost
    nr : ∀ {n} {x y} → (p : A)
        → heapgood x y p
        → (a : Heap x n full)
        → (b : Heap y n almost)
        → Heap (# p) (succ n) almost

  -- rightMost : ∀ {m h t s} → Heap m h t s → Path t
  -- rightMost leaf = end
  -- rightMost (nodel p x₁ h h₁) = {!!}
  -- rightMost (noder p x₁ h h₁) = {!!}
  -- rightMost (nodef p x₁ h h₁) = {!!}

  -- rank : ∀ {n p} → Heap p n → ℕ
  -- rank leaf = 0
  -- rank (node dr p dx dy x₁ x₂) = succ (rank x₂)

--  makeT : ∀ {n m x y} → (p : A) → leq (# p) x → leq (# p) y
--    → Heap x n → Heap y m → Heap (# p) (succ (min n m))
--  makeT {ra}{rb}{x}{y} p xp yp a b with cmpℕ {ra} {rb}
--  ... | tri< d e f = node (yes (orA d)) p yp xp b a
--  ... | tri= d e f rewrite e = node (yes (orB refl)) p xp yp a b
--  ... | tri> d e f = node (yes (orA f)) p xp yp a b

  -- ∼ ~ ⇒  ∷
--   union : ∀ {n m} → Heap n → Heap m → Heap (max n m)
--   union leaf leaf = leaf
--   union leaf (node d p a b) = node d p a b
--   union (node d p a b) leaf = node d p a b
--   union (node d1 p1 a1 b1) (node d2 p2 a2 b2) with cmp {p1} {p2}
--   ... | tri> x x₁ x₂ = {!!}
--   ... | _ = {!!}
--  union : Heap → Heap → Heap
--  union leaf leaf = leaf
--  union leaf b = b
--  union a leaf = a
--  union (node p1 l1 r1) (node p2 l2 r2) with less p1 p2
--  ... | yes a = makeT p1 l1 (union r1 (node p2 l2 r2))
--  ... | no ¬a = makeT p2 l2 (union r2 (node p1 l1 r1))
 
--  insert0 : A → Heap → Heap
--  insert0 x h = union (singleton x) h
--  insert : A → Heap → Heap
--  insert x leaf = singleton x
--  insert x (node p l r) = {! !}
--
--  data Maybe (A : Set) : Set where
--    nothing : Maybe A
--    just    : A → Maybe A
--  getMin : Heap → Maybe A
--  getMin leaf = nothing
--  getMin (node p _ _) = just p
--  exctractMin : Heap → (Maybe A) × Heap
--  exctractMin leaf = nothing , leaf
--  exctractMin (node p l r) = just p , union l r
