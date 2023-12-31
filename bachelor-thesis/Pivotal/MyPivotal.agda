module MyPivotal where

data Zero : Set where
record One : Set where
  constructor ⟨⟩ -- no fields
-- ⟨ == \<
-- ⟩ == \>
data Two : Set where
  tt ff : Two

So : Two → Set
So tt = One
So ff = Zero

record proven (P : Set) : Set where
  constructor !
  field {{prf}} : P

_⇒_ : Set → Set → Set
P ⇒ T = {{p : P}} → T
infixr 3 _⇒_

_∴_ : ∀ {P T} → proven P → (P ⇒ T) → T
! ∴ t = t

not : Two → Two
not tt = ff
not ff = tt

if_then_else_ : ∀ {X} b → (So b ⇒ X) → (So (not b) ⇒ X) → X
if_then_else_ tt t _ = t
if_then_else_ ff _ f = f
infix 1 if_then_else_

magic : {X : Set} → Zero ⇒ X
magic {{()}}

test : Two
test = if tt then ff else magic

data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

infixl 6 _ℕ+_
_ℕ+_ : ℕ → ℕ → ℕ
zero ℕ+ b = b
succ a ℕ+ b = succ (a ℕ+ b)

data _ℕ≤_ : ℕ → ℕ → Set where
  z≤n : ∀ {n} → zero ℕ≤ n
  s≤s : ∀ {n m} → n ℕ≤ m → succ n ℕ≤ succ m

_ℕ<_ _ℕ≥_ _ℕ>_ : ℕ → ℕ → Set
n ℕ< m = succ n ℕ≤ m
n ℕ> m = m ℕ< n
n ℕ≥ m = m ℕ≤ n

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

  ⊥-elim : ∀ {a} {A : Set a} → Zero → A
  ⊥-elim ()

  ¬ : ∀ {a} → Set a → Set a
  ¬ P = P → Zero

  private
   module DummyAB {α β} {A : Set α} {B : Set β} where
    contradiction : A → ¬ A → B
    contradiction a ¬a = ⊥-elim (¬a a)

    contraposition : (A → B) → (¬ B → ¬ A)
    contraposition = flip _∘′_

    contraposition¬ : (A → ¬ B) → (B → ¬ A)
    contraposition¬ = flip
  open DummyAB

open Logic public

module MLTT where
  infix 4 _≡_
  data _≡_ {a} {A : Set a} (x : A) : A → Set a where
    refl : x ≡ x

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

open MLTT public

module Decidable where
  data Dec {a} (A : Set a) : Set a where
    yes : ( a :   A) → Dec A
    no  : (¬a : ¬ A) → Dec A
open Decidable public

module BinarySearchTreeBad (P : Set) (le : P → P → Two) where

  data TrS : Set where
    leafS : TrS
    nodeS : TrS → P → TrS → TrS

  data STRange : Set where
    ∅ : STRange
    _─_ : P → P → STRange
  infix 9 _─_
  -- ─ == \---
  
  data Maybe (X : Set) : Set where
    no : Maybe X
    yes : X → Maybe X
    
  _?⟩_ : ∀ {X} → Two → Maybe X → Maybe X
  b ?⟩ mx = if b then mx else no
  infixr 4 _?⟩_

  valid : TrS → Maybe STRange
  valid leafS = yes ∅
  valid (nodeS l p r) with valid l | valid r
  ... | yes ∅ | yes ∅ = yes (p ─ p)
  ... | yes ∅ | yes (c ─ d) = le p c ?⟩ yes (p ─ d)
  ... | yes (a ─ b) | yes ∅ = le b p ?⟩ yes (a ─ p)
  ... | yes (a ─ b) | yes (c ─ d) =
    le b p ?⟩ le p c ?⟩ yes (a ─ d)
  ... | _ | _ = no
  
  lOK : STRange → P → Two
  lOK ∅ p = tt
  lOK (_ ─ b) p = le b p

  rOK : STRange → P → Two
  rOK ∅ p = tt
  rOK (a ─ _) p = le p a

  rOut : STRange → P → STRange → STRange
  rOut ∅ p ∅ = p ─ p
  rOut ∅ p (_ ─ d) = p ─ d
  rOut (a ─ _) p ∅ = a ─ p
  rOut (a ─ _) _ (_ ─ d) = a ─ d

  data BST : STRange → Set where
    leaf₁ : BST ∅
    node₁ : ∀ {lt rt} → BST lt → (p : P) → BST rt →
      So (lOK lt p) ⇒ So (rOK rt p) ⇒ BST (rOut lt p rt)

  insertS : P → TrS → TrS
  insertS y leafS = nodeS leafS y leafS
  insertS y (nodeS lt p rt) = if le y p
    then nodeS (insertS y lt) p rt
    else nodeS lt p (insertS y rt)

  oRange : STRange → P → STRange
  oRange ∅ x = x ─ x
  oRange (l ─ r) x = if le x l
    then x ─ r
    else (if le r x then l ─ x else (l ─ r))

  -- shite insert
  -- newInsert : ∀ {r} x → BST r → BST (oRange r x)
  -- newInsert x leaf₁ = node₁ leaf₁ x leaf₁
  -- newInsert x (node₁ l p r {{lp}} {{pr}}) = if le x p then node₁ {!newInsert x l!} p r else (node₁ {! l!} p {! newInsert x r!})
  
data expanded (P : Set) : Set where
  ⊤ : expanded P
  # : P → expanded P
  ⊥ : expanded P

expandRel : ∀ {P} → (P → P → Two) → (expanded P → expanded P → Two)
expandRel le _ ⊤ = tt
expandRel le ⊥ _ = tt
expandRel le (# x) (# y) = le x y
expandRel _ _ _ = ff

module BSTBetter where

  postulate
    P : Set
    le : P → P → Two

  data BST (l u : expanded P) : Set where 
    leaf : BST l u
    pnode : (p : P) → BST l (# p) → BST (# p) u →
      So ((expandRel le) l (# p)) ⇒ So ((expandRel le) (# p) u) ⇒ BST l u
  pattern node lp p pu = pnode p lp pu
  -- insert2 skipped here

-- section OWOTO

Rel : Set → Set1
Rel P = P × P → Set

leℕ : Rel ℕ
leℕ (x , y) = x ≤ y where
  _≤_ : ℕ → ℕ → Set
  zero ≤ y = One
  succ x ≤ zero = Zero
  succ x ≤ succ y = x ≤ y

data _+_ (S T : Set) : Set where
  ◅ : S → S + T
  ▻ : T → S + T
infixr 4 _+_
-- ▻ ◅ == \tw (5,6)

OWOTO : ∀ {P} (L : Rel P) → Rel P
OWOTO L (x , y) = (proven (L (x , y))) + (proven (L (y , x)))

pattern le = ◅ !
pattern ge = ▻ !

ℕowoto : ∀ x y → OWOTO leℕ (x , y)
ℕowoto zero y            = le
ℕowoto (succ x) zero     = ge
ℕowoto (succ x) (succ y) = ℕowoto x y

relExpanded provenRel : ∀ {P} → Rel P → Rel (expanded P)
relExpanded L (_ , ⊤) = One
relExpanded L (# x , # y) = L (x , y)
relExpanded L (⊥ , _) = One
relExpanded L ( _ , _ ) = Zero

-- prooven relation 
provenRel L = λ xy → proven (relExpanded L xy)

-- six 6

Never Always : {I : Set} → I → Set
Never  i = Zero
Always i = One

_+̇_ _×̇_ _→̇_ : {I : Set} → (I → Set) → (I → Set) → I → Set
(S +̇ T) i = S i + T i
(S ×̇ T) i = S i × T i
(S →̇ T) i = S i → T i

infixr 3 _+̇_
infixr 4 _×̇_
infixr 2 _→̇_

-- "always works" ≡ works for every index
[_] : {I : Set} → (I → Set) → Set
[ F ] = ∀ {i} → F i

test[] : ∀ {I} {S T : I → Set} → [ S →̇ S +̇ T ]
test[] = ◅

-- section "Working with Bounded Sets"

_∧̇_ : ∀ {P} → Rel (expanded P) → Rel (expanded P) → Rel (expanded P)
_∧̇_ {P} S T (l , u) = Σ P (λ p → S (l , # p) × T (# p , u))

pattern _&_&_ s p t = p , s , t
infixr 5 _&_&_

∙_ : ∀ {P} (L : Rel P) → Rel (expanded P)
∙ L = (provenRel L) ∧̇ (provenRel L)

-- owoto : ∀ x y → OWOTO L (x , y)
  
pattern interval' p = ! & p & !

module BSTWorks where
  postulate
    P : Set
    L : Rel P
    owoto : ∀ x y → OWOTO L (x , y)
   
  data BST (lu : (expanded P) × (expanded P)) : Set where
    leaf : BST lu
    pnode : (((provenRel L) ×̇ BST) ∧̇ ((provenRel L) ×̇ BST)
           →̇ BST) lu
    
  pattern node lt p rt = pnode (p , (! , lt) , (! , rt))

  -- skipped "BROWN" insert
  insert3 : [ (∙ L) →̇ BST →̇ BST ]
  insert3 (interval' y) leaf = node leaf y leaf
  insert3 (interval' y) (node lt p rt) with owoto y p
  ... | le = node (insert3 (interval' y) lt) p rt
  ... | ge = node lt p (insert3 (interval' y) rt)

-- section "Importance of Local Knowledge"
-- skipped rotR
module BSTBest where
  postulate
    P : Set
    L : Rel P
    owoto : ∀ x y → OWOTO L (x , y)
  
  data BST (lu : (expanded P) × (expanded P)) : Set where
    pleaf : ((provenRel L) →̇ BST) lu
    pnode : ((BST ∧̇ BST) →̇ BST) lu

  pattern leaf = pleaf !
  pattern node lt p rt = pnode (lt & p & rt)

  insert : [ (∙ L) →̇ BST →̇ BST ]
  insert (interval' y) leaf = node leaf y leaf
  insert (interval' y) (node lt p rt) with owoto y p
  ... | le = node (insert (interval' y) lt) p rt
  ... | ge = node lt p (insert (interval' y) rt)

  rotR : [ BST →̇ BST ]
  rotR (node (node A b C) d E) = node A b (node C d E)
  rotR t = t

  data OList (lu : (expanded P) × (expanded P)) : Set where
    nil : ((provenRel L) →̇ OList) lu
    cons : (((provenRel L) ∧̇ OList) →̇ OList) lu

data JJ : Set where
  `P `R `1 : JJ
  _`+_ _`×_ : JJ → JJ → JJ
infixr 4 _`+_
infixr 5 _`×_

⟦_⟧jj : JJ → Set → Set → Set
⟦_⟧jj `P R P = P
⟦_⟧jj `R R P = R
⟦_⟧jj `1 R P = One
⟦_⟧jj (S `+ T) R P = ⟦ S ⟧jj R P + ⟦ T ⟧jj R P
⟦_⟧jj (S `× T) R P = ⟦ S ⟧jj R P × ⟦ T ⟧jj R P

data μ₀ (F : JJ) (P : Set) : Set where
  ⟨_⟩ : ⟦ F ⟧jj (μ₀ F P) P → μ₀ F P
-- MuJJ

record Applicative (H : Set → Set) : Set₁ where
  field
    pure : ∀ {X} → X → H X
    ap   : ∀ {S T} → H (S → T) → H S → H T
open Applicative

traverse : ∀ {H F A B} → Applicative H → (A → H B) → μ₀ F A → H (μ₀ F B)
traverse {H} {F} {A} {B} AH h t = go `R t where
  pu = pure AH
  _⊛_ = ap AH
  go : ∀ G → ⟦ G ⟧jj (μ₀ F A) A → H (⟦ G ⟧jj (μ₀ F B) B)
  go `P s = h s
  go `R ⟨ x ⟩ = pu ⟨_⟩ ⊛ go F x
  go `1 ⟨⟩ = pu ⟨⟩
  go (S `+ T) (◅ s) = pu ◅ ⊛ go S s
  go (S `+ T) (▻ t) = pu ▻ ⊛ go T t
  go (S `× T) (s , t) = (pu _,_ ⊛ go S s) ⊛ go T t

-- read McBride & Paterson
idApp : Applicative (λ X → X)
idApp = record { pure = id; ap = id }

map : ∀ {F A B} → (A → B) → μ₀ F A → μ₀ F B
map = traverse idApp
 
record Monoid (X : Set) : Set where
  field
    neutral : X
    combine : X → X → X
  monApp : Applicative (λ _ → X)
  monApp = record { pure = λ _ → neutral ; ap = combine }
  crush : ∀ {P F} → (P → X) → μ₀ F P → X
  crush = traverse {B = Zero} monApp
open Monoid

compMon : ∀ {X} → Monoid (X → X)
compMon = record {neutral = id ; combine = λ f g → f ∘ g }

foldr : ∀ {F A B} → (A → (B → B)) → B → μ₀ F A → B
foldr f b t = crush compMon f t b

data SO : Set where
  `R `1 : SO
  _`+_ _`∧_ : SO → SO → SO
-- ∧ == \and
infixr 5 _`∧_

⟦_⟧so : SO → JJ
⟦ `R ⟧so = `R
⟦ `1 ⟧so = `1
⟦ S `+ T ⟧so = ⟦ S ⟧so `+ ⟦ T ⟧so
⟦ S `∧ T ⟧so = ⟦ S ⟧so `× `P `× ⟦ T ⟧so
-- ⟦ == \[[

-- MuSO
μ₁ : SO → Set → Set
μ₁ F P = μ₀ ⟦ F ⟧so P

`List-so `Tree-so `Interval-so : SO
`Tree-so = `1 `+ (`R `∧ `R)
`List-so = `1 `+ (`1 `∧ `R)
`Interval-so = `1 `∧ `1

tree₁ : ∀ {F A} → μ₁ F A → μ₁ `Tree-so A
tree₁ {F} {A} ⟨ t ⟩ = go F t where
  go : ∀ G → ⟦ ⟦ G ⟧so ⟧jj (μ₁ F A) A → μ₁ `Tree-so A
  go `R r = tree₁ r
  go `1 ⟨⟩ = ⟨ ◅ ⟨⟩ ⟩
  go (S `+ T) (◅ s) = go S s
  go (S `+ T) (▻ t) = go T t
  go (S `∧ T) (s , p , t) = ⟨ ▻ (go S s , p , go T t) ⟩


⟦_⟧so-≤ : SO → ∀ {P} → Rel (expanded P) → Rel P → Rel (expanded P)
⟦_⟧so-≤ `R R L = R
⟦_⟧so-≤ `1 R L = (provenRel L)
⟦_⟧so-≤ (S `+ T) R L = ⟦ S ⟧so-≤ R L +̇ ⟦ T ⟧so-≤ R L
⟦_⟧so-≤ (S `∧ T) R L = ⟦ S ⟧so-≤ R L ∧̇ ⟦ T ⟧so-≤ R L

-- MuOSO
data μ₂ (F : SO) {P : Set} (L : Rel P) (lu : (expanded P) × (expanded P)) : Set where
  ⟨_⟩ : ⟦ F ⟧so-≤ (μ₂ F L) L lu → μ₂ F L lu
  
Tree-so Interval-so List-so : ∀ {P} → Rel P → Rel (expanded P)
Tree-so = μ₂ `Tree-so
pattern leaf-so = ⟨ ◅ ! ⟩
pattern node-so lp p pu = ⟨ ▻ (lp & p & pu) ⟩

Interval-so = μ₂ `Interval-so
pattern interval p = ⟨ ( p , ! , ! ) ⟩

tree-so : ∀ {A F} {L : Rel A} → [ μ₂ F L →̇ Tree-so L ]
tree-so {A} {F} {L} ⟨ x ⟩ = go F x where
  go : ∀ G → [ ⟦ G ⟧so-≤ (μ₂ F L) L →̇ Tree-so L ]
  go `R t = tree-so t
  go `1 ! = leaf-so
  go (S `+ T) (◅ s) = go S s
  go (S `+ T) (▻ t) = go T t
  go (S `∧ T) (s & p & t) = node-so (go S s) p (go T t)

module TreeInsert {P : Set} (L : Rel P) (owoto : ∀ x y → OWOTO L (x , y)) where

  insert2 : [ Interval-so L →̇ Tree-so L →̇ Tree-so L ]
  insert2 (interval x) leaf-so = node-so leaf-so x leaf-so
  insert2 (interval x) (node-so lt p rt) with owoto x p
  ... | le = node-so (insert2 (interval x) lt) p rt
  ... | ge = node-so lt p (insert2 (interval x) rt)

  makeTree : ∀ {F} → μ₁ F P → Tree-so L (⊥ , ⊤)
  makeTree = foldr (λ x → insert2 ⟨ ! & x & ! ⟩) ⟨ ◅ ! ⟩

List-so L = μ₂ `List-so L

-- `List-so = `1 `+ (`1 `∧ `R)
pattern [] = ⟨ ◅ ! ⟩
pattern _∷_ x xs = ⟨ ▻ (x , ! , xs) ⟩
infixr 6 _∷_

module MergeSO where
  postulate
    P : Set
    L : Rel P
    owoto : ∀ x y → OWOTO L (x , y)
    
  merge : [ List-so L →̇ List-so L →̇ List-so L ]
  merge [] = id
  merge {l , u} (x ∷ xs) = go where
    go : ∀ {l} {{ _ : relExpanded L (l , # x) }} → (List-so L →̇ List-so L) (l , u)
    go [] = x ∷ xs
    go (y ∷ ys) with owoto x y
    ... | le = x ∷ merge xs (y ∷ ys)
    ... | ge = y ∷ go ys

  olMon : ∀ {lu} → relExpanded L lu ⇒ Monoid (List-so L lu)
  olMon = record {neutral = [] ; combine = merge }

  mergeJJ : ∀ {F} → μ₀ F P → List-so L (⊥ , ⊤)
  mergeJJ = crush olMon (λ p → p ∷ [])
  
  `LTree : JJ
  `LTree = (`1 `+ `P) `+ `R `× `R
  
  pattern none = ⟨ ◅ ( ◅ ⟨⟩ ) ⟩
  pattern one p = ⟨ ◅ ( ▻ p ) ⟩
  pattern fork l r = ⟨ ▻ ( l , r ) ⟩

  twistIn : P → μ₀ `LTree P → μ₀ `LTree P
  twistIn p none = one p
  twistIn p (one q) = fork (one p) (one q)
  twistIn p (fork l r) = fork (twistIn p r) l
  
  mergeSort : ∀ {F} → μ₀ F P → (List-so L) (⊥ , ⊤)
  mergeSort t = mergeJJ (foldr twistIn none t)

infixr 8 _++_

RepL : ∀ {P} → Rel P → Rel (expanded P)
RepL L (n , u) = ∀ {m} → relExpanded L (m , n) ⇒ List-so L (m , u)
_++_  : ∀ {P} {L : Rel P} {l n u} → List-so L (l , n) → RepL L (n , u) → List-so L (l , u)
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ xs ++ ys

flapp : ∀ {P} {L : Rel P} {F} {l n u} → μ₂ F L (l , n) → RepL L (n , u) → List-so L (l , u)
flapp {P} {L} {F} {u = u} t ys = go `R t ys where
  go : ∀ {l n} G → ⟦ G ⟧so-≤ (μ₂ F L) L (l , n) → RepL L (n , u) → List-so L (l , u)
  go `R ⟨ x ⟩ xs = go F x xs
  go `1 ! xs = xs
  go (S `+ T) (◅ s) xs = go S s xs
  go (S `+ T) (▻ t) xs = go T t xs
  go (S `∧ T) (s & p & t) xs = go S s (p ∷ go T t xs)
   
flatten : ∀ {P} {L : Rel P} {F} → [ μ₂ F L →̇ List-so L ]
flatten s = flapp s []

-- Indexed Orderable
data IO (I : Set) : Set where
  `R    : I → IO I
  `0 `1 : IO I
  _`+_ _`∧_ : IO I → IO I → IO I
  
⟦_⟧-io : ∀ {I P} → IO I → (I → Rel (expanded P)) → Rel P → Rel (expanded P)
⟦_⟧-io (`R x) R L = R x
⟦_⟧-io `0 R L = λ _ → Zero
⟦_⟧-io `1 R L = provenRel L
⟦_⟧-io (S `+ T) R L = ⟦ S ⟧-io R L +̇ ⟦ T ⟧-io R L
⟦_⟧-io (S `∧ T) R L = ⟦ S ⟧-io R L ∧̇ ⟦ T ⟧-io R L

data μ₃ {I P : Set} (F : I → IO I) (L : Rel P) (i : I) (lu : (expanded P) × (expanded P)) : Set where
  ⟨_⟩ : ⟦ F i ⟧-io (μ₃ F L) L lu → μ₃ F L i lu

`List-io `Tree-io `Interval-io : One → IO One
`List-io     _ = `1 `+ (`1 `∧ `R ⟨⟩)
`Tree-io     _ = `1 `+ (`R ⟨⟩ `∧ `R ⟨⟩)
`Interval-io _ = `1 `∧ `1

List-io Tree-io Interval-io : ∀ {P} → Rel P → Rel (expanded P)
List-io     L = μ₃ `List-io L ⟨⟩
Tree-io     L = μ₃ `Tree-io L ⟨⟩
Interval-io L = μ₃ `Interval-io L ⟨⟩

`Vec : ℕ → IO ℕ
`Vec zero = `1
`Vec (succ n) = `1 `∧ `R n

pattern leaf-io = ⟨ ◅ ! ⟩
pattern node-io lt p rt = ⟨ ▻ (p , lt , rt) ⟩

tree-io : ∀ {I P F} {L : Rel P} {i : I} →
  [ (μ₃ F L i) →̇ (Tree-io L) ]
tree-io {F = F} {L = L} {i = i} ⟨ x ⟩ = go (F i) x where
  go : ∀ G → [ ⟦ G ⟧-io (μ₃ F L) L →̇ Tree-io L ]
  go (`R i) t = tree-io t
  go `0 ()
  go `1 ! = leaf-io
  go (S `+ T) (◅ s) = go S s
  go (S `+ T) (▻ t) = go T t
  go (S `∧ T) (lt & p & rt) = node-io (go S lt) p (go T rt)

pattern _∷'_ x xs = ⟨ ▻ (x , ! , xs) ⟩
flatten-io : ∀ {I P F} {L : Rel P} {i : I} → [ μ₃ F L i →̇ List-io L ]
flatten-io {I} {P} {F} {L} {i} {l , u} ⟨ x ⟩ = go (F i) x ⟨ ◅ ! ⟩ where
  go : ∀ G {l n} → ⟦ G ⟧-io (μ₃ F L) L (l , n) →
    (∀ {m} → (relExpanded L) (m , n) ⇒ List-io L (m , u)) → List-io L (l , u)
  go (`R i)   ⟨ t ⟩ ys = go (F i) t ys
  go `0       ()    ys
  go `1       !     ys = ys
  go (S `+ T) (◅ s) ys = go S s ys
  go (S `+ T) (▻ t) ys = go T t ys
  go (S `∧ T) (s & p & t) ys = go S s (p ∷' (go T t ys))

 
-- Modules TryHeap*
-- references
-- http://www.cs.ru.nl/dtp11/slides/capretta.pdf -- Section "Inductive-Recursive Leftist Heaps"
-- C. Okasaki "PURELY FUNCTIONAL DATA STRUCTURES" Section 3.1 "Leftist Heaps"
-- http://hackage.haskell.org/package/heap-1.0.0/docs/Data-Heap.html
-- haskell heap package is based on leftist-heaps from Okasaki's book


data Tri {α β γ} (A : Set α) (B : Set β) (C : Set γ) : Set (α ⊔ (β ⊔ γ)) where
  tri< : ( a :   A) (¬b : ¬ B) (¬c : ¬ C) → Tri A B C
  tri= : (¬a : ¬ A) ( b :   B) (¬c : ¬ C) → Tri A B C
  tri> : (¬a : ¬ A) (¬b : ¬ B) ( c :   C) → Tri A B C
-- ℕ>
cmpℕ : (a b : ℕ) → Tri (a ℕ< b) (a ≡ b) (b ℕ< a)
cmpℕ a b = ?
--min max : ℕ → ℕ → ℕ
---- min x y = if x ℕ≤ y then x else y
---- max x y = if x ℕ≥ y then x else y


module TryHeap3 (A : Set) (_<_ _==_ : A → A → Set) (cmp : (a b : A) → Tri (a < b) (a == b) (b < a)) where

  Rel₂ : Set → Set₁
  Rel₂ A = A → A → Set
  
  flip₁ : ∀ {A B : Set} {C : Set₁} → (A → B → C) → B → A → C
  flip₁ f a b = f b a
  
  less : (a b : A) → Dec (b < a)
  less a b with cmp a b
  less a b | tri< y y' y0 = no y0
  less a b | tri= y y' y0 = no y0
  less a b | tri> y y' y0 = yes y0
  
  eq : (a b : A) → Dec (a == b)
  eq a b with cmp a b
  eq a b | tri< y y' y0 = no y'
  eq a b | tri= y y' y0 = yes y'
  eq a b | tri> y y' y0 = no y' 
  
  more : (a b : A) → Dec (a < b)
  more a b with cmp a b
  more a b | tri< y y' y0 = yes y
  more a b | tri= y y' y0 = no y
  more a b | tri> y y' y0 = no y

  data Heap : ℕ → Set where
    leaf : Heap zero
    node : ∀ {n m} → A → Heap n → Heap m → Heap (succ (min n m))

--  rank : Heap → ℕ
--  rank leaf = zero
--  rank (node a left right) = succ (rank right)

  one : ℕ
  one = succ zero
  singleton : A → Heap one
  singleton p = node p leaf leaf

  makeT : ∀ {n m} → (p : A) → Heap n → Heap m → Heap (succ (min n m))
  makeT {ra} {rb} p a b = if ra ℕ> rb
    then node p a b
    else node p {! b!} a

--
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
