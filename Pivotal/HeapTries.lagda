\begin{code}
module HeapTries where

data ⊥ : Set where

record ⊤ : Set where
  constructor ⟨⟩

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

    open DummyA public
    open DummyAB public
  open ≡-Prop public

open MLTT public

Rel₂ : Set → Set₁
Rel₂ A = A → A → Set
 
data Tri {A : Set} (_<_ _==_ _>_ : Rel₂ A) (a b : A) : Set where
  tri< :   (a < b) → ¬ (a == b) → ¬ (a > b) → Tri _<_ _==_ _>_ a b
  tri= : ¬ (a < b) →   (a == b) → ¬ (a > b) → Tri _<_ _==_ _>_ a b
  tri> : ¬ (a < b) → ¬ (a == b) →   (a > b) → Tri _<_ _==_ _>_ a b
 
flip₁ : ∀ {A B : Set} {C : Set₁} → (A → B → C) → B → A → C
flip₁ f a b = f b a

Cmp : {A : Set} → Rel₂ A → Rel₂ A → Set
Cmp {A} _<_ _==_ = ∀ (x y : A) → Tri (_<_) (_==_) (flip₁ _<_) x y

data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ
{-# BUILTIN NATURAL ℕ #-}
{-# BUILTIN ZERO zero #-}
{-# BUILTIN SUC succ #-}

data _ℕ≤_ : Rel₂ ℕ where
  z≤n : ∀ {n} → zero ℕ≤ n
  s≤s : ∀ {n m} → n ℕ≤ m → succ n ℕ≤ succ m

_ℕ<_ _ℕ≥_ _ℕ>_ : Rel₂ ℕ
n ℕ< m = succ n ℕ≤ m
n ℕ> m = m ℕ< n
n ℕ≥ m = m ℕ≤ n

lemma-succ-≡ : ∀ {n} {m} → succ n ≡ succ m → n ≡ m
lemma-succ-≡ refl = refl
lemma-succ-≤ : ∀ {n} {m} → succ (succ n) ℕ≤ succ m → succ n ℕ≤ m
lemma-succ-≤ (s≤s r) = r

cmpℕ : Cmp {ℕ} _ℕ<_ _≡_
cmpℕ zero (zero) = tri= (λ ()) refl (λ ())
cmpℕ zero (succ y) = tri< (s≤s z≤n) (λ ()) (λ ())
cmpℕ (succ x) zero = tri> (λ ()) (λ ()) (s≤s z≤n)
cmpℕ (succ x) (succ y) with cmpℕ x y
... | tri<  a ¬b ¬c = tri< (s≤s a) (contraposition lemma-succ-≡ ¬b) (contraposition lemma-succ-≤ ¬c)
... | tri> ¬a ¬b  c = tri> (contraposition lemma-succ-≤ ¬a) (contraposition lemma-succ-≡ ¬b) (s≤s c)
... | tri= ¬a  b ¬c = tri= (contraposition lemma-succ-≤ ¬a) (cong succ b) (contraposition lemma-succ-≤ ¬c)

Trans : {A : Set} → Rel₂ A → Set
Trans {A} _rel_ = {a b c : A} → (a rel b) → (b rel c) → (a rel c)

data OR (A B : Set) : Set where
  orA : A → OR A B
  orB : B → OR A B

min max : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A} → (cmp : Cmp _<_ _==_) → A → A → A
min cmp x y with cmp x y
... | tri< _ _ _ = x
... | _ = y
max cmp x y with cmp x y
... | tri> _ _ _ = x
... | _ = y

infixr 4 _⇒_
-- Implication/containment. Could also be written ⊆.
_⇒_ : ∀ {A : Set} → Rel₂ A → Rel₂ A → Set
P ⇒ Q = ∀ {i j} → P i j → Q i j

-- Generalised symmetry.
Sym : ∀ {A : Set} → Rel₂ A → Rel₂ A → Set
Sym P Q = P ⇒ flip Q

Symmetric : ∀ {A : Set} → Rel₂ A → Set
Symmetric _∼_ = Sym _∼_ _∼_

_Respects_ : ∀ {ℓ} {A : Set} → (A → Set ℓ) → Rel₂ A → Set _
P Respects _∼_ = ∀ {x y} → x ∼ y → P x → P y

_Respects₂_ : ∀ {A : Set} → Rel₂ A → Rel₂ A → Set
P Respects₂ _∼_ =
  (∀ {x} → P x      Respects _∼_) ×
  (∀ {y} → flip P y Respects _∼_)

data _<=_ {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A} : Rel₂ A where
  le : ∀ {x y} → x < y → x <= y
  eq : ∀ {x y} → x == y → x <= y
  
lemma-<=min : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A} {cmp : Cmp _<_ _==_} {a b c : A} → (_<=_ {_<_ = _<_} {_==_} a b) → (_<=_ {_<_ = _<_} {_==_} a c)
  → (_<=_ {_<_ = _<_} {_==_} a (min cmp b c))
lemma-<=min {cmp = cmp} {_} {b} {c} ab ac with cmp b c
... | tri< _ _ _ = ab
... | tri= _ _ _ = ac
... | tri> _ _ _ = ac

min3 : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A} → (cmp : Cmp _<_ _==_) → A → A → A → A
min3 cmp x y z with cmp x y
... | tri< _ _ _ = min cmp x z
... | _ = min cmp y z

lemma-<=min3 : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A} {cmp : Cmp _<_ _==_} {x a b c : A}
  → (_<=_ {_<_ = _<_} {_==_} x a)
  → (_<=_ {_<_ = _<_} {_==_} x b)
  → (_<=_ {_<_ = _<_} {_==_} x c)
  → (_<=_ {_<_ = _<_} {_==_} x (min3 cmp a b c))
lemma-<=min3 {cmp = cmp} {x} {a} {b} {c} xa xb xc with cmp a b
... | tri< _ _ _ = lemma-<=min {cmp = cmp} xa xc
... | tri= _ _ _ = lemma-<=min {cmp = cmp} xb xc
... | tri> _ _ _ = lemma-<=min {cmp = cmp} xb xc
  
resp<= : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A} → (resp : _<_ Respects₂ _==_) → (trans== : Trans _==_) → (sym== : Symmetric _==_) → (_<=_ {A}{_<_}{_==_}) Respects₂ _==_
resp<= {A}{_<_}{_==_} resp trans sym = left , right where
  left : ∀ {a b c : A} → b == c → a <= b → a <= c
  left b=c (le a<b) = le (fst resp b=c a<b)
  left b=c (eq a=b) = eq (trans a=b b=c)
  right : ∀ {a b c : A} → b == c → b <= a → c <= a
  right b=c (le a<b) = le (snd resp b=c a<b)
  right b=c (eq a=b) = eq (trans (sym b=c) a=b)

trans<= : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A}
  → _<_ Respects₂ _==_ → Symmetric _==_ → Trans _==_ → Trans _<_
  → Trans (_<=_ {A}{_<_}{_==_})
trans<= r s t== t< (le a<b) (le b<c) = le (t< a<b b<c)
trans<= r s t== t< (le a<b) (eq b=c) = le (fst r b=c a<b)
trans<= r s t== t< (eq a=b) (le b<c) = le (snd r (s a=b) b<c)
trans<= r s t== t< (eq a=b) (eq b=c) = eq (t== a=b b=c)

module TryHeap (A : Set) (_<_ _==_ : Rel₂ A) (cmp : Cmp _<_ _==_)
  (sym== : Symmetric _==_) (resp : _<_ Respects₂ _==_) (trans< : Trans _<_)
  (trans== : Trans _==_)
  where

  data expanded (A : Set) : Set where
    # : A → expanded A
    top : expanded A
  
  data _<E_ : Rel₂ (expanded A) where
    base : ∀ {x y : A} → x < y → (# x) <E (# y)
    ext  : ∀ {x : A} → (# x) <E top
  lemma-<E : ∀ {x} {y} → (# x) <E (# y) → x < y
  lemma-<E (base r) = r
  trans<E : Trans _<E_
  trans<E {# _} {# _} {# _} a<b b<c = base (trans< (lemma-<E a<b) (lemma-<E b<c))
  trans<E {# _} {# _} {top} _  _  = ext
  trans<E {# _} {top} {_}   _  ()
  trans<E {top} {_}   {_}   () _

  data _=E_ : Rel₂ (expanded A) where
    base : ∀ {x y} → x == y → (# x) =E (# y)
    ext  : top =E top
  sym=E   : Symmetric _=E_
  sym=E (base a=b) = base (sym== a=b)
  sym=E ext = ext
  trans=E : Trans _=E_
  trans=E (base a=b) (base b=c) = base (trans== a=b b=c)
  trans=E ext ext = ext
  lemma-=E : ∀ {x} {y} → (# x) =E (# y) → x == y
  lemma-=E (base r) = r

  respE : _<E_ Respects₂ _=E_
  respE = left , right where
    left : ∀ {a b c : expanded A} → b =E c → a <E b → a <E c
    left {# _} {# _} {# _} (base r1) (base r2) = base (fst resp r1 r2)
    left {# _} {top} {top} ext ext = ext
    left {_} {# _} {top} () _
    left {_} {top} {# _} () _
    left {top} {_} {_}   _ ()
    right : ∀ {a b c : expanded A} → b =E c → b <E a → c <E a
    right {# _} {# _} {# _} (base r1) (base r2) = base (snd resp r1 r2)
    right {top} {# _} {# _} _ ext = ext
    right {_} {# _} {top} () _
    right {_} {top} {_} _ ()

  _≤_ : Rel₂ (expanded A)
  _≤_ = _<=_ {expanded A} {_<E_} {_=E_}

  trans≤ : Trans _≤_
  trans≤ = trans<= respE sym=E trans=E trans<E
  resp≤ : _≤_ Respects₂ _=E_
  resp≤ = resp<= respE trans=E sym=E

  cmpE : Cmp {expanded A} _<E_ _=E_
  cmpE (# x) (# y) with cmp x y
  cmpE (# x) (# y) | tri< a b c = tri< (base a) (contraposition lemma-=E b) (contraposition lemma-<E c)
  cmpE (# x) (# y) | tri= a b c = tri= (contraposition lemma-<E a) (base b) (contraposition lemma-<E c)
  cmpE (# x) (# y) | tri> a b c = tri> (contraposition lemma-<E a) (contraposition lemma-=E b) (base c)
  cmpE (# x) top = tri< ext (λ ()) (λ ())
  cmpE top (# y) = tri> (λ ()) (λ ()) ext
  cmpE top top   = tri= (λ ()) ext (λ ())

  minE : (x y : expanded A) → expanded A
  minE = min cmpE

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
        → (a : Heap x (succ n) full)
        → (b : Heap y (succ n) almost)
        → Heap (# p) (succ (succ n)) almost
  peekMin : ∀ {m h s} → Heap m h s → (expanded A)
  peekMin eh = top
  peekMin (nd p _ _ _ _) = # p
  peekMin (nf p _ _ _ _) = # p
  peekMin (nl p _ _ _ _) = # p
  peekMin (nr p _ _ _ _) = # p
  lemma-<=minE : ∀ {a b c} → a ≤ b → a ≤ c → a ≤ (minE b c)
  lemma-<=minE ab ac = lemma-<=min {expanded A}{_<E_}{_=E_}{cmpE} ab ac

  min3E : (expanded A) → (expanded A) → (expanded A) → (expanded A)
  min3E x y z = min3 cmpE x y z
  lemma-<=min3E : ∀ {x a b c} → x ≤ a → x ≤ b → x ≤ c → x ≤ (min3E a b c)
  lemma-<=min3E = lemma-<=min3 {expanded A}{_<E_}{_=E_}{cmpE}

  finsert : ∀ {h m} → (z : A) → Heap m h full
    → Σ HeapState (Heap (minE m (# z)) (succ h))
  finsert {0} z eh = full , nf z (le ext) (le ext) eh eh
  finsert {1} z (nf p i j eh eh) with cmp p z
  ... | tri< p<z _ _ = almost , nd p (le (base p<z)) j (nf z (le ext) (le ext) eh eh) eh
  ... | tri= _ p=z _ = almost , nd z (eq (base (sym== p=z))) (le ext) (nf p i j eh eh) eh
  ... | tri> _ _ z<p = almost , nd z (le (base z<p)) (le ext) (nf p i j eh eh) eh
  finsert z (nf p i j (nf x i₁ j₁ a b) c) with cmp p z
  finsert z (nf p i j (nf x i₁ j₁ a b) c) | tri< p<z _ _
    with finsert z (nf x i₁ j₁ a b)
    | lemma-<=minE {# p} {# x} {# z} i (le (base p<z))
  ... | full   , newleft | l1 = almost , nd p l1 j newleft c
  ... | almost , newleft | l1 = almost , nl p l1 j newleft c
  finsert z (nf p i j (nf x i₁ j₁ a b) c) | tri= _ p=z _
    with finsert p (nf x i₁ j₁ a b)
    | lemma-<=minE {# z} {# x} {# p} (snd resp≤ (base p=z) i) (eq (base (sym== p=z))) | snd resp≤ (base p=z) j
  ... | full   , newleft | l1 | l2 = almost , nd z l1 l2 newleft c
  ... | almost , newleft | l1 | l2 = almost , nl z l1 l2 newleft c
  finsert z (nf p i j (nf x i₁ j₁ a b) c) | tri> _ _ z<p
    with finsert p (nf x i₁ j₁ a b)
    | lemma-<=minE {# z} {# x} {# p} (trans≤ (le (base z<p)) i) (le (base z<p))
  ... | full   , newleft | l1 = almost , nd z l1 (trans≤ (le (base z<p)) j) newleft c
  ... | almost , newleft | l1 = almost , nl z l1 (trans≤ (le (base z<p)) j) newleft c
  
  ainsert : ∀ {h m} → (z : A) → Heap m h almost
    → Σ HeapState (Heap (minE m (# z)) h)
  ainsert z (nd p i j a b) with cmp p z
  ainsert z (nd p i j a b) | tri< p<z _ _ with finsert z b | lemma-<=minE j (le (base p<z))
  ... | full   , nb | l1 = full   , nf p i l1 a nb
  ... | almost , nb | l1 = almost , nr p i l1 a nb
  ainsert z (nd p i j a b) | tri= _ p=z _ with finsert p b | snd resp≤ (base p=z) i | lemma-<=minE (snd resp≤ (base p=z) j) (eq (base (sym== p=z)))
  ... | full   , nb | l1 | l2 = full   , nf z l1 l2 a nb
  ... | almost , nb | l1 | l2 = almost , nr z l1 l2 a nb
  ainsert z (nd p i j a b) | tri> _ _ z<p with finsert p b | trans≤ (le (base z<p)) i | lemma-<=minE (trans≤ (le (base z<p)) j) (le (base z<p))
  ... | full   , nb | l1 | l2 = full   , nf z l1 l2 a nb
  ... | almost , nb | l1 | l2 = almost , nr z l1 l2 a nb

  ainsert z (nl p i j a b) with cmp p z
  ainsert z (nl p i j a b) | tri< p<z _ _ with ainsert z a | lemma-<=minE i (le (base p<z))
  ... | full   , na | l1 = almost , nd p l1 j na b
  ... | almost , na | l1 = almost , nl p l1 j na b
  ainsert z (nl p i j a b) | tri= _ p=z _ with ainsert p a | lemma-<=minE (snd resp≤ (base p=z) i) (eq (base (sym== p=z))) | snd resp≤ (base p=z) j
  ... | full   , na | l1 | l2 = almost , nd z l1 l2 na b
  ... | almost , na | l1 | l2 = almost , nl z l1 l2 na b
  ainsert z (nl p i j a b) | tri> _ _ z<p with ainsert p a | lemma-<=minE (trans≤ (le (base z<p)) i) (le (base z<p)) | trans≤ (le (base z<p)) j
  ... | full   , na | l1 | l2 = almost , nd z l1 l2 na b
  ... | almost , na | l1 | l2 = almost , nl z l1 l2 na b

  ainsert z (nr p i j a b) with cmp p z
  ainsert z (nr p i j a b) | tri< p<z _ _ with ainsert z b | lemma-<=minE j (le (base p<z))
  ... | full   , nb | l1 = full   , nf p i l1 a nb
  ... | almost , nb | l1 = almost , nr p i l1 a nb
  ainsert z (nr p i j a b) | tri= _ p=z _ with ainsert p b | snd resp≤ (base p=z) i | lemma-<=minE (snd resp≤ (base p=z) j) (eq (base (sym== p=z)))
  ... | full   , nb | l1 | l2 = full   , nf z l1 l2 a nb
  ... | almost , nb | l1 | l2 = almost , nr z l1 l2 a nb
  ainsert z (nr p i j a b) | tri> _ _ z<p with ainsert p b | trans≤ (le (base z<p)) i | lemma-<=minE (trans≤ (le (base z<p)) j) (le (base z<p))
  ... | full   , nb | l1 | l2 = full   , nf z l1 l2 a nb
  ... | almost , nb | l1 | l2 = almost , nr z l1 l2 a nb
  
  fmerge : ∀ {x y h} → Heap x h full → Heap y h full → OR (Heap x zero full × (x ≡ y) × (h ≡ zero)) (Heap (minE x y) (succ h) almost)
  fmerge eh eh = orA (eh , refl , refl)
  fmerge (nf x i₁ j₁ a b) (nf y i₂ j₂ c d) with cmp x y
  fmerge (nf x i₁ j₁ a b) (nf y i₂ j₂ c d) | tri< x<y _ _ with fmerge a b
  ... | orA (eh , refl , refl) = orB (nd x (le (base x<y)) j₁ (nf y i₂ j₂ c d) eh)
  ... | orB ab = orB (nr x (le (base x<y)) (lemma-<=minE i₁ j₁) (nf y i₂ j₂ c d) ab)
  fmerge (nf x i₁ j₁ a b) (nf y i₂ j₂ c d) | tri= _ x=y _ with fmerge c d
  ... | orA (eh , refl , refl) = orB (nd y (eq (base (sym== x=y))) j₂ (nf x i₁ j₁ a b) eh)
  ... | orB cd = orB (nr y (eq (base (sym== x=y))) (lemma-<=minE i₂ j₂) (nf x i₁ j₁ a b) cd) 
  fmerge (nf x i₁ j₁ a b) (nf y i₂ j₂ c d) | tri> _ _ y<x with fmerge c d
  ... | orA (eh , refl , refl) = orB (nd y (le (base y<x)) j₂ (nf x i₁ j₁ a b) eh)
  ... | orB cd = orB (nr y (le (base y<x)) (lemma-<=minE i₂ j₂) (nf x i₁ j₁ a b) cd)

  fpop : ∀ {m h} → Heap m (succ h) full
    → OR (Σ (expanded A) (λ x → (Heap x (succ h) almost) × (m ≤ x))) (Heap top h full)
  fpop (nf _ _ _ eh eh) = orB eh
  fpop (nf _ i j (nf x i₁ j₁ a b) (nf y i₂ j₂ c d)) with fmerge (nf x i₁ j₁ a b) (nf y i₂ j₂ c d)
  ... | orA (() , _ , _)
  ... | orB res = orA ((minE (# x) (# y)) , res , lemma-<=minE i j)

  makeH : ∀ {x y h} → (p : A) → Heap x h full → Heap y h full → Heap (min3E x y (# p)) (succ h) full
  makeH p eh eh = nf p (le ext) (le ext) eh eh
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) with cmp x y
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri< x<y _ _ with cmp x p
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri< x<y _ _ | tri< x<p _ _ with makeH p a b
  ... | res = nf x (lemma-<=min3E i j (le (base x<p))) (le (base x<y)) res (nf y i₁ j₁ c d)
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri< x<y _ _ | tri= _ x=p _ = nf p (eq (base (sym== x=p))) (le (base (snd resp x=p x<y))) (nf x i j a b) (nf y i₁ j₁ c d)
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri< x<y _ _ | tri> _ _ p<x = nf p (le (base p<x)) (le (base (trans< p<x x<y))) (nf x i j a b) (nf y i₁ j₁ c d)

  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ with cmp y p
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | tri< y<p _ _ = nf y (eq (base (sym== x=y))) (lemma-<=min3E i₁ j₁ (le (base y<p))) (nf x i j a b) (makeH p c d)
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | tri= _ y=p _ = nf p (eq (base (trans== (sym== y=p) (sym== x=y)))) (eq (base (sym== y=p))) (nf x i j a b) (nf y i₁ j₁ c d)
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | tri> _ _ p<y = nf p (le (base (fst resp (sym== x=y) p<y))) (le (base p<y)) (nf x i j a b) (nf y i₁ j₁ c d)

  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri> _ _ y<x with cmp y p
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri> _ _ y<x | tri< y<p _ _ = nf y (le (base y<x)) (lemma-<=min3E i₁ j₁ (le (base y<p))) (nf x i j a b) (makeH p c d)
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri> _ _ y<x | tri= _ y=p _ = nf p (le (base (snd resp y=p y<x))) (eq (base (sym== y=p))) (nf x i j a b) (nf y i₁ j₁ c d)
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri> _ _ y<x | tri> _ _ p<y = nf p (le (base (trans< p<y y<x))) (le (base p<y)) (nf x i j a b) (nf y i₁ j₁ c d)

  lemma-resp : ∀ {x y a b} → x == y → (# x) ≤ a → (# x) ≤ b → (# y) ≤ minE a b
  lemma-resp x=y i j = lemma-<=minE (snd resp≤ (base x=y) i) (snd resp≤ (base x=y) j)
  lemma-trans : ∀ {x y a b} → y < x → (# x) ≤ a → (# x) ≤ b → (# y) ≤ minE a b
  lemma-trans y<x i j = lemma-<=minE (trans≤ (le (base y<x)) i) (trans≤ (le (base y<x)) j)

  ndmerge : ∀ {x y h} → Heap x (succ (succ h)) full → Heap y (succ h) full
    → Heap (minE x y) (succ (succ (succ h))) almost
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) with cmp x y
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri< x<y _ _ with fmerge a b
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri< x<y _ _ | orA (_ , _ , ())
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri< x<y _ _ | orB x₁ = nl x (lemma-<=minE i j) (le (base x<y)) x₁ (nf y i₁ j₁ c d)

  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ with fmerge c d
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | orA (eh , refl , refl) with fmerge a b
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | orA (eh , refl , refl) | orA (eh , refl , ())
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | orA (eh , refl , refl) | orB ab = nl y (lemma-resp x=y i j) (eq (base (sym== x=y))) ab (nf x (le ext) (le ext) eh eh)

  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | orB cd with fmerge a b
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | orB cd | orA (_ , _ , ())
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | orB cd | orB ab = nl y (lemma-resp x=y i j) (lemma-<=min3E i₁ j₁ (eq (base (sym== x=y)))) ab (makeH x c d)
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri> _ _ y<x with fmerge a b
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri> _ _ y<x | orA (_ , _ , ())
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri> _ _ y<x | orB ab = nl y (lemma-trans y<x i j) (lemma-<=min3E i₁ j₁ (le (base y<x))) ab (makeH x c d)

  afmerge : ∀ {h x y} → Heap x (succ (succ h)) almost
    → OR (Heap y (succ h) full) (Heap y (succ (succ h)) full)
    → OR (Heap (minE x y) (succ (succ h)) full) (Heap (minE x y) (succ (succ (succ h))) almost)
  afmerge (nd x i j (nf p i₁ j₁ eh eh) eh) (orA (nf y i₂ j₂ eh eh)) with cmp x y
  ... | tri< x<y _ _ = orA (nf x i (le (base x<y)) (nf p i₁ j₁ eh eh) (nf y i₂ j₂ eh eh))
  ... | tri= _ x=y _ = orA (nf y (eq (base (sym== x=y))) (snd resp≤ (base x=y) i) (nf x (le ext) (le ext) eh eh) (nf p i₁ j₁ eh eh))
  ... | tri> _ _ y<x = orA (nf y (le (base y<x)) (trans≤ (le (base y<x)) i) (nf x j j eh eh) (nf p j₁ j₁ eh eh))

  afmerge (nd x i j (nf p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orA (nf y i₃ j₃ c d)) with cmp x y | ndmerge (nf p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)
  ... | tri< x<y _ _ | ab = orB (nl x (lemma-<=minE i j) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri= _ x=y _ | ab = orB (nl y (lemma-resp x=y i j) (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri> _ _ y<x | ab = orB (nl y (lemma-trans y<x i j) (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))
  afmerge (nl x i j (nd p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orA (nf y i₃ j₃ c d)) with cmp x y | afmerge (nd p₁ i₁ j₁ a₁ b₁) (orA (nf p₂ i₂ j₂ a₂ b₂))
  ... | tri< x<y _ _ | orA ab = orA (nf x (lemma-<=minE i j) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri< x<y _ _ | orB ab = orB (nl x (lemma-<=minE i j) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri= _ x=y _ | orA ab = orA (nf y (lemma-resp x=y i j) (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri= _ x=y _ | orB ab = orB (nl y (lemma-resp x=y i j) (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri> _ _ y<x | orA ab = orA (nf y (lemma-trans y<x i j) (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))
  ... | tri> _ _ y<x | orB ab = orB (nl y (lemma-trans y<x i j) (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))

  afmerge (nl x i j (nl p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orA (nf y i₃ j₃ c d)) with cmp x y | afmerge (nl p₁ i₁ j₁ a₁ b₁) (orA (nf p₂ i₂ j₂ a₂ b₂))
  ... | tri< x<y _ _ | orA ab = orA (nf x (lemma-<=minE i j) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri< x<y _ _ | orB ab = orB (nl x (lemma-<=minE i j) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri= _ x=y _ | orA ab = orA (nf y (lemma-resp x=y i j) (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri= _ x=y _ | orB ab = orB (nl y (lemma-resp x=y i j) (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri> _ _ y<x | orA ab = orA (nf y (lemma-trans y<x i j) (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))
  ... | tri> _ _ y<x | orB ab = orB (nl y (lemma-trans y<x i j) (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))

  afmerge (nl x i j (nr p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orA (nf y i₃ j₃ c d)) with cmp x y | afmerge (nr p₁ i₁ j₁ a₁ b₁) (orA (nf p₂ i₂ j₂ a₂ b₂))
  ... | tri< x<y _ _ | orA ab = orA (nf x (lemma-<=minE i j) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri< x<y _ _ | orB ab = orB (nl x (lemma-<=minE i j) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri= _ x=y _ | orA ab = orA (nf y (lemma-resp x=y i j) (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri= _ x=y _ | orB ab = orB (nl y (lemma-resp x=y i j) (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri> _ _ y<x | orA ab = orA (nf y (lemma-trans y<x i j) (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))
  ... | tri> _ _ y<x | orB ab = orB (nl y (lemma-trans y<x i j) (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))

  afmerge (nr x i j (nf p₁ i₁ j₁ a₁ b₁) (nd p₂ i₂ j₂ a₂ b₂)) (orA (nf y i₃ j₃ c d)) with cmp x y | afmerge (nd p₂ i₂ j₂ a₂ b₂) (orB (nf p₁ i₁ j₁ a₁ b₁))
  ... | tri< x<y _ _ | (orA ab) = orA (nf x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab)
  ... | tri< x<y _ _ | (orB ab) = orB (nl x (lemma-<=minE j i) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri= _ x=y _ | (orA ab) = orA (nf y (lemma-resp x=y j i) (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri= _ x=y _ | (orB ab) = orB (nl y (lemma-resp x=y j i) (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri> _ _ y<x | (orA ab) = orA (nf y (lemma-trans y<x j i) (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))
  ... | tri> _ _ y<x | (orB ab) = orB (nl y (lemma-trans y<x j i) (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))

  afmerge (nr x i j (nf p₁ i₁ j₁ a₁ b₁) (nl p₂ i₂ j₂ a₂ b₂)) (orA (nf y i₃ j₃ c d)) with cmp x y | afmerge (nl p₂ i₂ j₂ a₂ b₂) (orB (nf p₁ i₁ j₁ a₁ b₁))
  ... | tri< x<y _ _ | (orA ab) = orA (nf x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab)
  ... | tri< x<y _ _ | (orB ab) = orB (nl x (lemma-<=minE j i) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri= _ x=y _ | (orA ab) = orA (nf y (lemma-resp x=y j i) (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri= _ x=y _ | (orB ab) = orB (nl y (lemma-resp x=y j i) (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri> _ _ y<x | (orA ab) = orA (nf y (lemma-trans y<x j i) (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))
  ... | tri> _ _ y<x | (orB ab) = orB (nl y (lemma-trans y<x j i) (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))

  afmerge (nr x i j (nf p₁ i₁ j₁ a₁ b₁) (nr p₂ i₂ j₂ a₂ b₂)) (orA (nf y i₃ j₃ c d)) with cmp x y | afmerge (nr p₂ i₂ j₂ a₂ b₂) (orB (nf p₁ i₁ j₁ a₁ b₁))
  ... | tri< x<y _ _ | (orA ab) = orA (nf x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab)
  ... | tri< x<y _ _ | (orB ab) = orB (nl x (lemma-<=minE j i) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri= _ x=y _ | (orA ab) = orA (nf y (lemma-resp x=y j i) (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri= _ x=y _ | (orB ab) = orB (nl y (lemma-resp x=y j i) (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri> _ _ y<x | (orA ab) = orA (nf y (lemma-trans y<x j i) (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))
  ... | tri> _ _ y<x | (orB ab) = orB (nl y (lemma-trans y<x j i) (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))

  afmerge (nd x i j (nf p i₁ j₁ eh eh) eh) (orB (nf y i₂ j₂ c d)) with cmp x y
  ... | tri< x<y _ _ = orB (nd x (le (base x<y)) i (nf y i₂ j₂ c d) (nf p i₁ j₁ eh eh))
  ... | tri= _ x=y _ = orB (nd y (lemma-<=min3E i₂ j₂ (eq (base (sym== x=y)))) (snd resp≤ (base x=y) i) (makeH x c d) (nf p i₁ j₁ eh eh))
  ... | tri> _ _ y<x = orB (nd y (lemma-<=min3E i₂ j₂ (le (base y<x))) (trans≤ (le (base y<x)) i) (makeH x c d) (nf p i₁ j₁ eh eh))

  afmerge (nd x i j (nf p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orB (nf y i₃ j₃ c d)) with cmp x y | ndmerge (nf p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)
  ... | tri< x<y _ _ | ab = orB (nr x (le (base x<y)) (lemma-<=minE i j) (nf y i₃ j₃ c d) ab)
  ... | tri= _ x=y _ | ab = orB (nr y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y i j) (makeH x c d) ab)
  ... | tri> _ _ y<x | ab = orB (nr y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x i j) (makeH x c d) ab)

  afmerge (nl x i j (nd p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orB (nf y i₃ j₃ c d)) with cmp x y | afmerge (nd p₁ i₁ j₁ a₁ b₁) (orA (nf p₂ i₂ j₂ a₂ b₂))
  ... | tri< x<y _ _ | (orA ab) = orB (nd x (le (base x<y)) (lemma-<=minE i j) (nf y i₃ j₃ c d) ab)
  ... | tri< x<y _ _ | (orB ab) = orB (nr x (le (base x<y)) (lemma-<=minE i j) (nf y i₃ j₃ c d) ab)
  ... | tri= _ x=y _ | (orA ab) = orB (nd y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y i j) (makeH x c d) ab)
  ... | tri= _ x=y _ | (orB ab) = orB (nr y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y i j) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orA ab) = orB (nd y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x i j) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orB ab) = orB (nr y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x i j) (makeH x c d) ab)
  afmerge (nl x i j (nl p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orB (nf y i₃ j₃ c d)) with cmp x y | afmerge (nl p₁ i₁ j₁ a₁ b₁) (orA (nf p₂ i₂ j₂ a₂ b₂))
  ... | tri< x<y _ _ | (orA ab) = orB (nd x (le (base x<y)) (lemma-<=minE i j) (nf y i₃ j₃ c d) ab)
  ... | tri< x<y _ _ | (orB ab) = orB (nr x (le (base x<y)) (lemma-<=minE i j) (nf y i₃ j₃ c d) ab)
  ... | tri= _ x=y _ | (orA ab) = orB (nd y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y i j) (makeH x c d) ab)
  ... | tri= _ x=y _ | (orB ab) = orB (nr y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y i j) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orA ab) = orB (nd y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x i j) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orB ab) = orB (nr y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x i j) (makeH x c d) ab)

  afmerge (nl x i j (nr p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orB (nf y i₃ j₃ c d)) with cmp x y | afmerge (nr p₁ i₁ j₁ a₁ b₁) (orA (nf p₂ i₂ j₂ a₂ b₂))
  ... | tri< x<y _ _ | (orA ab) = orB (nd x (le (base x<y)) (lemma-<=minE i j) (nf y i₃ j₃ c d) ab)
  ... | tri< x<y _ _ | (orB ab) = orB (nr x (le (base x<y)) (lemma-<=minE i j) (nf y i₃ j₃ c d) ab)
  ... | tri= _ x=y _ | (orA ab) = orB (nd y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y i j) (makeH x c d) ab)
  ... | tri= _ x=y _ | (orB ab) = orB (nr y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y i j) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orA ab) = orB (nd y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x i j) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orB ab) = orB (nr y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x i j) (makeH x c d) ab)
  afmerge (nr x i j (nf p₁ i₁ j₁ a₁ b₁) (nd p₂ i₂ j₂ a₂ b₂)) (orB (nf y i₃ j₃ c d)) with cmp x y | afmerge (nd p₂ i₂ j₂ a₂ b₂) (orB (nf p₁ i₁ j₁ a₁ b₁)) 
  ... | tri< x<y _ _ | (orA ab) = orB (nd x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab) 
  ... | tri< x<y _ _ | (orB ab) = orB (nr x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab)
  ... | tri= _ x=y _ | (orA ab) = orB (nd y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y j i) (makeH x c d) ab)
  ... | tri= _ x=y _ | (orB ab) = orB (nr y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y j i) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orA ab) = orB (nd y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x j i) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orB ab) = orB (nr y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x j i) (makeH x c d) ab)
  afmerge (nr x i j (nf p₁ i₁ j₁ a₁ b₁) (nl p₂ i₂ j₂ a₂ b₂)) (orB (nf y i₃ j₃ c d)) with cmp x y | afmerge (nl p₂ i₂ j₂ a₂ b₂) (orB (nf p₁ i₁ j₁ a₁ b₁)) 
  ... | tri< x<y _ _ | (orA ab) = orB (nd x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab) 
  ... | tri< x<y _ _ | (orB ab) = orB (nr x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab)
  ... | tri= _ x=y _ | (orA ab) = orB (nd y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y j i) (makeH x c d) ab)
  ... | tri= _ x=y _ | (orB ab) = orB (nr y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y j i) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orA ab) = orB (nd y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x j i) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orB ab) = orB (nr y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x j i) (makeH x c d) ab)
  afmerge (nr x i j (nf p₁ i₁ j₁ a₁ b₁) (nr p₂ i₂ j₂ a₂ b₂)) (orB (nf y i₃ j₃ c d)) with cmp x y | afmerge (nr p₂ i₂ j₂ a₂ b₂) (orB (nf p₁ i₁ j₁ a₁ b₁)) 
  ... | tri< x<y _ _ | (orA ab) = orB (nd x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab) 
  ... | tri< x<y _ _ | (orB ab) = orB (nr x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab)
  ... | tri= _ x=y _ | (orA ab) = orB (nd y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y j i) (makeH x c d) ab)
  ... | tri= _ x=y _ | (orB ab) = orB (nr y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y j i) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orA ab) = orB (nd y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x j i) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orB ab) = orB (nr y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x j i) (makeH x c d) ab)
 
  apop : ∀ {m h} → Heap m (succ h) almost
    → OR (Σ (expanded A) (λ x → (Heap x (succ h) almost) × (m ≤ x)))
         (Σ (expanded A) (λ x → (Heap x h full) × (m ≤ x)))
  apop (nd {x = x} p i j a eh) = orB (x , a , i)
  apop (nd _ i j (nf x i₁ j₁ a b) (nf y i₂ j₂ c d)) with cmp x y | ndmerge (nf x i₁ j₁ a b) (nf y i₂ j₂ c d)
  ... | tri< _ _ _ | res = orA (# x , res , i)
  ... | tri= _ _ _ | res = orA (# y , res , j)
  ... | tri> _ _ _ | res = orA (# y , res , j)
  apop (nl _ i j (nd x i₁ j₁ (nf y _ _ eh eh) eh) (nf z _ _ eh eh)) with cmp x z
  ... | tri< x<z _ _ = orB (# x , nf x i₁ (le (base x<z)) (nf y (le ext) (le ext) eh eh) (nf z (le ext) (le ext) eh eh) , i)
  ... | tri= _ x=z _ = orB (# z , nf z (eq (base (sym== x=z))) (snd resp≤ (base x=z) i₁) (nf x (le ext) (le ext) eh eh) (nf y (le ext) (le ext) eh eh) , j)
  ... | tri> _ _ z<x = orB (# z , nf z (le (base z<x)) (trans≤ (le (base z<x)) i₁) (nf x (le ext) (le ext) eh eh) (nf y (le ext) (le ext) eh eh) , j)

  apop (nl _ i j (nd x i₁ j₁ (nf y i₂ j₂ a₂ b₂) (nf z i₃ j₃ a₃ b₃)) (nf t i₄ j₄ c d)) with cmp x t | ndmerge (nf y i₂ j₂ a₂ b₂) (nf z i₃ j₃ a₃ b₃)
  ... | tri< x<t _ _ | res = orA (# x , nl x (lemma-<=minE i₁ j₁) (le (base x<t)) res (nf t i₄ j₄ c d) , i)
  ... | tri= _ x=t _ | res = orA (# t , nl t (snd resp≤ (base x=t) (lemma-<=minE i₁ j₁)) (lemma-<=min3E i₄ j₄ (eq (base (sym== x=t)))) res (makeH x c d) , j)
  ... | tri> _ _ t<x | res = orA (# t , nl t (lemma-trans t<x i₁ j₁) (lemma-<=min3E i₄ j₄ (le (base t<x))) res (makeH x c d) , j)

  apop (nl _ i j (nl x i₁ j₁ a b) (nf y i₂ j₂ c d)) with cmp x y | afmerge (nl x i₁ j₁ a b) (orA (nf y i₂ j₂ c d))
  ... | tri< x<y _ _ | orA res = orB (# x , res , i)
  ... | tri= _ x=y _ | orA res = orB (# y , res , j)
  ... | tri> _ _ y<x | orA res = orB (# y , res , j)
  ... | tri< x<y _ _ | orB res = orA (# x , res , i)
  ... | tri= _ x=y _ | orB res = orA (# y , res , j)
  ... | tri> _ _ y<x | orB res = orA (# y , res , j)
  apop (nl _ i j (nr x i₁ j₁ a b) (nf y i₂ j₂ c d)) with cmp x y | afmerge (nr x i₁ j₁ a b) (orA (nf y i₂ j₂ c d))
  ... | tri< x<y _ _ | orA res = orB (# x , res , i)
  ... | tri= _ x=y _ | orA res = orB (# y , res , j)
  ... | tri> _ _ y<x | orA res = orB (# y , res , j)
  ... | tri< x<y _ _ | orB res = orA (# x , res , i)
  ... | tri= _ x=y _ | orB res = orA (# y , res , j)
  ... | tri> _ _ y<x | orB res = orA (# y , res , j)
  apop (nr _ i j (nf x i₁ j₁ a b) (nd y i₂ j₂ c d)) with cmp y x | afmerge (nd y i₂ j₂ c d) (orB (nf x i₁ j₁ a b))
  ... | tri< y<x _ _ | orA res = orB (# y , res , j)
  ... | tri= _ y=x _ | orA res = orB (# x , res , i)
  ... | tri> _ _ x<y | orA res = orB (# x , res , i)
  ... | tri< y<x _ _ | orB res = orA (# y , res , j)
  ... | tri= _ y=x _ | orB res = orA (# x , res , i)
  ... | tri> _ _ x<y | orB res = orA (# x , res , i)
  apop (nr _ i j (nf x i₁ j₁ a b) (nl y i₂ j₂ c d)) with cmp y x | afmerge (nl y i₂ j₂ c d) (orB (nf x i₁ j₁ a b))
  ... | tri< y<x _ _ | orA res = orB (# y , res , j)
  ... | tri= _ y=x _ | orA res = orB (# x , res , i)
  ... | tri> _ _ x<y | orA res = orB (# x , res , i)
  ... | tri< y<x _ _ | orB res = orA (# y , res , j)
  ... | tri= _ y=x _ | orB res = orA (# x , res , i)
  ... | tri> _ _ x<y | orB res = orA (# x , res , i)
  apop (nr _ i j (nf x i₁ j₁ a b) (nr y i₂ j₂ c d)) with cmp y x | afmerge (nr y i₂ j₂ c d) (orB (nf x i₁ j₁ a b))
  ... | tri< y<x _ _ | orA res = orB (# y , res , j)
  ... | tri= _ y=x _ | orA res = orB (# x , res , i)
  ... | tri> _ _ x<y | orA res = orB (# x , res , i)
  ... | tri< y<x _ _ | orB res = orA (# y , res , j)
  ... | tri= _ y=x _ | orB res = orA (# x , res , i)
  ... | tri> _ _ x<y | orB res = orA (# x , res , {!!})

\end{code}
\AgdaHide{
\begin{code}
\end{code}
}
