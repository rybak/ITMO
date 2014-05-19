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

cmpℕ : Cmp {ℕ} _ℕ<_ _≡_ -- {x y : ℕ} → Tri _ℕ<_ _≡_ _ℕ>_ x y
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

equal : {A : Set} {_<_ _==_ : Rel₂ A} → Cmp _<_ _==_ → (a b : A) → Dec (a == b)
equal cmp a b with cmp {a} {b}
equal cmp a b | tri< y y' y0 = no y'
equal cmp a b | tri= y y' y0 = yes y'
equal cmp a b | tri> y y' y0 = no y' 

more : {A : Set} {_<_ _==_ : Rel₂ A} → Cmp _<_ _==_ → (a b : A) → Dec (b < a)
more cmp a b with cmp {a} {b}
more cmp a b | tri< y y' y0 = no y0
more cmp a b | tri= y y' y0 = no y0
more cmp a b | tri> y y' y0 = yes y0

Trans : {A : Set} → Rel₂ A → Set
Trans {A} _rel_ = {a b c : A} → (a rel b) → (b rel c) → (a rel c)

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

min max : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A} → (cmp : Cmp _<_ _==_) → A → A → A
min cmp x y with cmp {x} {y}
... | tri< _ _ _ = x
... | _ = y
max cmp x y with cmp {x} {y}
... | tri> _ _ _ = x
... | _ = y
minℕ maxℕ : ℕ → ℕ → ℕ
minℕ = min cmpℕ
maxℕ = max cmpℕ

Reflexive : ∀ {A : Set} → Rel₂ A → Set
Reflexive _∼_ = ∀ {x} → x ∼ x

-- Irreflexivity is defined using an underlying equality.
Irreflexive : ∀ {A : Set} → Rel₂ A → Rel₂ A → Set
Irreflexive _≈_ _<_ = ∀ {x y} → x ≈ y → ¬ (x < y)

infixr 4 _⇒_
-- Implication/containment. Could also be written ⊆.
_⇒_ : ∀ {A : Set} → Rel₂ A → Rel₂ A → Set
P ⇒ Q = ∀ {i j} → P i j → Q i j

-- Generalised symmetry.
Sym : ∀ {A : Set} → Rel₂ A → Rel₂ A → Set
Sym P Q = P ⇒ flip Q

Symmetric : ∀ {A : Set} → Rel₂ A → Set
Symmetric _∼_ = Sym _∼_ _∼_

Asymmetric : ∀ {A : Set} → Rel₂ A → Set
Asymmetric _<_ = ∀ {x y} → x < y → ¬ (y < x)

_Respects_ : ∀ {ℓ} {A : Set} → (A → Set ℓ) → Rel₂ A → Set _
P Respects _∼_ = ∀ {x y} → x ∼ y → P x → P y

_Respects₂_ : ∀ {A : Set} → Rel₂ A → Rel₂ A → Set
P Respects₂ _∼_ =
  (∀ {x} → P x      Respects _∼_) ×
  (∀ {y} → flip P y Respects _∼_)

lemma-min-sym : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A} {cmp : Cmp _<_ _==_} {x y : A} → (r : Reflexive _==_) → min cmp x y == min cmp y x
lemma-min-sym {cmp = cmp} {x = x} {y = y} r with cmp {x} {y} | cmp {y} {x}
lemma-min-sym r | tri< x₁ x₂ x₃ | tri< x₄ x₅ x₆ = contradiction x₁ x₆
lemma-min-sym r | tri< x₁ x₂ x₃ | tri= x₄ x₅ x₆ = contradiction x₁ x₆
lemma-min-sym r | tri< x₁ x₂ x₃ | tri> x₄ x₅ x₆ = r
lemma-min-sym r | tri= x₁ x₂ x₃ | tri< x₄ x₅ x₆ = r
lemma-min-sym r | tri= x₁ x₂ x₃ | tri= x₄ x₅ x₆ = x₅
lemma-min-sym r | tri= x₁ x₂ x₃ | tri> x₄ x₅ x₆ = contradiction x₁ (λ z → z x₆)
lemma-min-sym r | tri> x₁ x₂ x₃ | tri< x₄ x₅ x₆ = r
lemma-min-sym r | tri> x₁ x₂ x₃ | tri= x₄ x₅ x₆ = x₅
lemma-min-sym r | tri> x₁ x₂ x₃ | tri> x₄ x₅ x₆ = contradiction x₁ (λ z → z x₆)

lemma-min2 : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A} {cmp : Cmp _<_ _==_} {x y : A} → x < y → (min cmp x y) < y
lemma-min2 {cmp = cmp} {x} {y} l with cmp {x} {y}
lemma-min2 l | tri< x₁ x₂ x₃ = l
lemma-min2 l | tri= x₁ x₂ x₃ = contradiction x₂ (λ _ → x₁ l)
lemma-min2 l | tri> x₁ x₂ x₃ = contradiction l x₁

lemma-min≡ : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A} {cmp : Cmp _<_ _==_} {x y : A} → x < y → (min cmp x y) ≡ x
lemma-min≡ {cmp = cmp}{x}{y} l with cmp {x}{y}
lemma-min≡ l | tri< x₁ x₂ x₃ = refl
lemma-min≡ l | tri= x₁ x₂ x₃ = contradiction x₂ (λ _ → x₁ l)
lemma-min≡ l | tri> x₁ x₂ x₃ = contradiction l x₁

data _<=_ {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A} : Rel₂ A where
  les : ∀ {x y} → x < y → x <= y
  equ : ∀ {x y} → x == y → x <= y
  
module TryHeap (A : Set) (_<_ _==_ : Rel₂ A) (cmp : Cmp _<_ _==_)
  (sym== : Symmetric _==_) (resp : _<_ Respects₂ _==_) (trans< : Trans _<_)
  (trans== : Trans _==_)
  where

  maxA minA : A → A → A
  maxA = max cmp
  minA = min cmp
  data expanded (A : Set) : Set where
    # : A → expanded A
    top : expanded A
  
  data _<E_ : Rel₂ (expanded A) where
    base : ∀ {x} {y} → x < y → (# x) <E (# y)
    ext  : ∀ {x} → (# x) <E top
    
  lemma-<E : ∀ {x} {y} → (# x) <E (# y) → x < y
  lemma-<E (base r) = r
  transE : Trans _<E_
  transE {# x} {# x₁} {# x₂} a<b b<c = base (trans< (lemma-<E a<b) (lemma-<E b<c))
  transE {# x} {# x₁} {top} a<b b<c = ext
  transE {# x} {top} {# x₁} a<b ()
  transE {# x} {top} {top} a<b ()
  transE {top} {# x} {# x₁} () b<c
  transE {top} {# x} {top} () b<c
  transE {top} {top} {# x} () b<c
  transE {top} {top} {top} () b<c

  data _=E_ : Rel₂ (expanded A) where
    base : ∀ {x y} → x == y → (# x) =E (# y)
    ext  : top =E top
  lemma-=E : ∀ {x} {y} → (# x) =E (# y) → x == y
  lemma-=E (base r) = r

  data _≤_ : Rel₂ (expanded A) where
    le : ∀ {x} {y} → x <E y → x ≤ y
    eq : ∀ {x} {y} → x =E y → x ≤ y
  _<=E_ : Rel₂ (expanded A)
  _<=E_ = _<=_ {expanded A} {_<E_} {_=E_}

  postulate
    trans2 : Trans _≤_

  cmpE : Cmp {expanded A} _<E_ _=E_
  cmpE {# x} {# y} with cmp {x} {y}
  cmpE {# x} {# y} | tri< a b c = tri< (base a) (contraposition lemma-=E b) (contraposition lemma-<E c)
  cmpE {# x} {# y} | tri= a b c = tri= (contraposition lemma-<E a) (base b) (contraposition lemma-<E c)
  cmpE {# x} {# y} | tri> a b c = tri> (contraposition lemma-<E a) (contraposition lemma-=E b) (base c)
  cmpE {# x} {top} = tri< ext (λ ()) (λ ())
  cmpE {top} {# y} = tri> (λ ()) (λ ()) ext
  cmpE {top} {top} = tri= (λ ()) ext (λ ())

  leq : (a b : expanded A) → Set
  leq a b = Dec (OR (a <E b) (a =E b))
  
  minE : (x y : expanded A) → expanded A
  minE = min cmpE
  maxE : (x y : expanded A) → expanded A
  maxE = max cmpE

  heapgood : (a b : expanded A) → (p : A) → Set
  heapgood a b p = (leq (# p) a) × (leq (# p) b)
   
  data HeapState : Set where
    full almost : HeapState

  data Heap : (expanded A) → (h : ℕ) → HeapState → Set where
    eh : Heap top zero full
    nd : ∀ {n} {x y} → (p : A) → (i : (# p) ≤ x) → (j : (# p) ≤ y)
        → (a : Heap x (succ n) full)
        → (b : Heap y n full)
        → Heap (# p) (succ (succ n)) almost
    nf : ∀ {n} {x y} → (p : A) → (i : (# p) ≤ x) → (j : (# p) ≤ y)
        → (a : Heap x n full)
        → (b : Heap y n full)
        → Heap (# p) (succ n) full
    nl : ∀ {n} {x y} → (p : A) → (i : (# p) ≤ x) → (j : (# p) ≤ y)
        → (a : Heap x (succ n) almost)
        → (b : Heap y n full)
        → Heap (# p) (succ (succ n)) almost
    nr : ∀ {n} {x y} → (p : A) → (i : (# p) ≤ x) → (j : (# p) ≤ y)
        → (a : Heap x n full)
        → (b : Heap y n almost)
        → Heap (# p) (succ n) almost
  root : ∀ {m h s} → Heap m h s → (expanded A)
  root eh = top
  root (nd p i j h h₁) = # p
  root (nf p i j h h₁) = # p
  root (nl p i j h h₁) = # p
  root (nr p i j h h₁) = # p
  lemma-min-min : ∀ x y → (# (minA x y)) ≡ minE (# x) (# y)
  lemma-min-min x y with cmp {x} {y}
  lemma-min-min x y | tri< x₁ x₂ x₃ = refl
  lemma-min-min x y | tri= x₁ x₂ x₃ = refl
  lemma-min-min x y | tri> x₁ x₂ x₃ = refl

  lemma-exist-less : ∀ {x y} → x <E y → Σ A (λ i → (# i) ≡ x)
  lemma-exist-less {x = # a} (base x₁) = a , refl
  lemma-exist-less {x = # a} ext = a , refl

  respE : _<E_ Respects₂ _=E_
  respE = left , right where
    left : {a b c : expanded A} → b =E c → a <E b → a <E c
    left {# a} {# b} {# c} (base r1) (base r2) = base (fst resp r1 r2)
    left {# a} {# b} {top} () _
    left {# a} {top} {# c} () _
    left {# a} {top} {top} ext ext = ext
    left {top} {# b} {# c} _ ()
    left {top} {# b} {top} () _
    left {top} {top} {# c} () _
    left {top} {top} {top} _ ()
    right : {a b c : expanded A} → b =E c → b <E a → c <E a
    right {# a} {# b} {# c} (base r1) (base r2) = base (snd resp r1 r2)
    right {# a} {# b} {top} () _
    right {# a} {top} {# c} () _
    right {# a} {top} {top} _ ()
    right {top} {# b} {# c} _ ext = ext
    right {top} {# b} {top} () _
    right {top} {top} {# c} () _
    right {top} {top} {top} _ ()

--   new-root : ∀ {p1 p2 x y} → heapgood x y p1 → p2 < p1 → heapgood x y p2
--   new-root (yes (orA x₁) , yes (orA x₂)) r = yes (orA (transE (base r) x₁)) , yes (orA (transE (base r) x₂))
--   new-root (yes (orA x₁) , yes (orB x₂)) r = yes (orA (transE (base r) x₁)) , yes (orA (fst respE x₂ (base r)))
--   new-root (yes (orB x₁) , yes (orA x₂)) r = yes (orA (fst respE x₁ (base r))) , yes (orA (transE (base r) x₂))
--   new-root (yes (orB x₁) , yes (orB x₂)) r = yes (orA (fst respE x₁ (base r))) , yes (orA (fst respE x₂ (base r)))
--   new-root {p1}{p2}{x}{y} (yes (orA x₁) , no nb) r with cmpE {# p2} {y}
--   new-root (yes (orA x₄) , no nb) r | tri< x x₁ x₂ = {! !}
--   new-root (yes (orA x₄) , no nb) r | tri= x x₁ x₂ = yes (orA (transE (base r) x₄)) , no (contradiction (snd respE x₁ (base r)) {! lemma-m1 nb!})
--   new-root (yes (orA x₄) , no nb) r | tri> x x₁ x₂ = {!!}
--   new-root (yes (orB x₁) , no nb) r = {!!}
--   new-root (no na , yes b) r = {!!}
--   new-root (no na , no nb) r = {!!}

  swap-root : ∀ {p1 p2 h s} → p2 <E p1 → Heap p1 (succ h) s → Heap p2 (succ h) s
  swap-root {p1} {top} () x
  swap-root {p1 = # p1}{p2 = # p2} r (nd .(p1) i j H H₁) = nd p2 {!!} {!!} H H₁
  swap-root {p1 = # p1}{p2 = # p2} r (nf .(p1) i j H H₁) = {!!}
  swap-root {p1 = # p1}{p2 = # p2} r (nl .(p1) i j H H₁) = {!!}
  swap-root {p1 = # p1}{p2 = # p2} r (nr .(p1) i j H H₁) = {!!}

  finsert : ∀ {m h} → (z : A) → Heap m h full
    → Σ HeapState (Heap (minE m (# z)) (succ h))
  finsert {h = 0} z eh = full , nf z (le ext) (le ext) eh eh
  finsert {h = 1} z (nf p i j eh eh) with cmp {p} {z}
  ... | tri< z₁ z₂ z₃ = almost , nd p (le (base z₁)) j (nf z (le ext) (le ext) eh eh) eh
  ... | tri= z₁ z₂ z₃ = almost , nd z (eq (base (sym== z₂))) (le ext) (nf p i j eh eh) eh
  ... | tri> z₁ z₂ z₃ = almost , nd z (le (base z₃)) (le ext) (nf p i j eh eh) eh
  finsert z (nf p i j (nf pp ii jj a b) c) with cmp {p} {z}
  finsert z (nf p i j (nf pp ii jj a b) c) | tri< x x₁ x₂ with finsert z (nf pp ii jj a b)
  ... | full , snd = almost , nd p {!!} j snd c
  ... | almost , snd = {!!}
  finsert z (nf p i j (nf pp ii jj a b) c) | tri= x x₁ x₂ = {!!}
  finsert z (nf p i j (nf pp ii jj a b) c) | tri> x x₁ x₂ = {!!}
--   finsert {.(# p)} {succ (succ n)} z (nf p i j (nf .{n} pp ii jj a b) c) | tri< x x₁ x₂ with finsert z (nf pp ii jj a b)
--   finsert {.(# p)} {succ (succ n)} z (nf p x₄ x (nf j ii jj a b) c) | tri< x₁ x₂ x₃ | full , snd = almost , nd p {!!} x snd c
--   finsert {.(# p)} {succ (succ n)} z (nf p x₄ x (nf j ii jj a b) c) | tri< x₁ x₂ x₃ | almost , snd = {!!}
--   finsert {.(# p)} {succ (succ n)} z (nf p i j (nf pp ii jj a b) c) | tri= x x₁ x₂ = {!!}
--   finsert {.(# p)} {succ (succ n)} z (nf p i j (nf pp ii jj a b) c) | tri> x x₁ x₂ = {!!}
  --finsert z (nf p i j (nf pp ii jj a b) c) with finsert z (nf pp ii jj a b)
  --finsert z (nf p n₁ n (nf j ii jj a b) c) | full , snd = ?
  --finsert z (nf p n₁ n (nf j ii jj a b) c) | almost , snd = ?

--  finsert {.(# p)} {succ (succ n)} z (nf p i j (nf pp ii jj a b) c) | full , snd | r = ?
  -- finsert {.(# p)} {succ (succ n)} z (nf p i j (nf pp ii jj a b) c) | almost , snd = {!!}





