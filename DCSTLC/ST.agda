{-
  Author: Jan Malakhovski
  Date: Nov 17, 2013

  ? : Andrey Rybak 
  Date: 17.11.2013 — 28.11.2013

-}

-- An excercise on de Bruijn Church-style Simply Typed Lambda Calculus
-- and its properties.
module ST where

open import OXIj.BrutalDepTypes
open Data-List
open Data-Any
open ≡-Prop

module TransReflClosure where
  -- Transitive reflexive closure of a relation
  data _✴ {A} (R : A → A → Set) : A → A → Set where
    ε  : {a : A} → (R ✴) a a -- Reflexivity
    _∷✴_ : {a b c : A} → R a b → (R ✴) b c → (R ✴) a c
  -- Structurally ─ a list with two indexes, where each cons
  -- connects them by relation R.

  -- Transitivity
  _++✴_ : ∀ {A} {R : A → A → Set} {a b c} → (R ✴) a b → (R ✴) b c → (R ✴) a c
  ε ++✴ bs = bs
  (a ∷✴ as) ++✴ bs = a ∷✴ (as ++✴ bs)

  -- Closure of a closure is simply a closure
  concat✴ : ∀ {A} {R : A → A → Set} {x y} → ((R ✴) ✴) x y → (R ✴) x y
  concat✴ ε = ε
  concat✴ (y' ∷✴ y0) = y' ++✴ concat✴ y0

  -- General map between closures
  -- Comprehensible when (u ≡ id).
  map✴ : ∀ {A B} {R : A → A → Set} {S : B → B → Set} (u : A → B)
       → (∀ {x y} → R x y → S (u x) (u y))
       → ∀ {x y} → (R ✴) x y → (S ✴) (u x) (u y)
  map✴ u f ε = ε
  map✴ u f (y ∷✴ y') = f y ∷✴ map✴ u f y'

open TransReflClosure public

module GeneralizedCurchRosser where
  -- Confluent relation
  Confluent : ∀ {A} (R : A → A → Set) → Set
  Confluent {A} R = ∀ {a b c} → R a b → R a c → Σ A λ d → R b d ∧ R c d

  {- Given a, b, c, R a b, and R a c
        R
      a - b
     R|
      c
     Gives d, R b d, and R c d

         b
         ⋮ R
     c ⋯ d
       R
     to complete the square
     a - b
     |   ⋮
     c ⋯ d
  -}

  module Proof where
    -- column building
    yCR : ∀ {A} {R : A → A → Set}
        → Confluent R → ∀ {a b c}
        → R a b → (R ✴) a c
        → Σ A λ d → (R ✴) b d ∧ R c d
    yCR cR {a} {b} {.a} Rab ε = b , ε , Rab
    yCR cR Rab (x ∷✴ xs) with cR Rab x
    yCR cR Rab (x ∷✴ xs) | d , (Rbd , Rb₁d) with yCR cR Rb₁d xs
    ... | cont = projl cont , Rbd ∷✴ fst (projr cont) , snd (projr cont)
    {-
    a  → b
    ↓    ↓
    b₁ → d   ⌉
    ↓        | result of cont
    ↓        |
    c  → ?   ⌋
    -}
    -- concatenating columns into a table
    xCR : ∀ {A} {R : A → A → Set} → Confluent R → Confluent (R ✴)
    xCR cR {a} {.a} {c} ε R*ac = c , R*ac , ε
    xCR cR {a} {b} {.a} (Rab ∷✴ R*bb) ε = b , ε , Rab ∷✴ R*bb
    xCR cR (Rab₁ ∷✴ R*b₁b) (Rac₁ ∷✴ R*c₁c) with yCR cR Rab₁ (Rac₁ ∷✴ R*c₁c)
    ... | column with xCR cR R*b₁b (fst (projr column))
    ... | cont = projl cont , fst (projr cont) , snd (projr column) ∷✴ snd (projr cont)

  -- Confluent relations have confluent closures
  CR : ∀ {A} {R : A → A → Set} → Confluent R → Confluent (R ✴)
  CR = Proof.xCR

open GeneralizedCurchRosser public

module SimptyTypedLambdaCalculusAtomicallyTypedWith (T : Set) where
  -- Simple types
  infixr 5 _→'_
  data Type : Set where
    ✶_   : T → Type
    _→'_ : Type → Type → Type

  -- Context
  Ctx : Set
  Ctx = List Type

  open Membership {A = Type} _≡_

  -- Simply typed λ-term
  data Term (Γ : Ctx) : Type → Set where
    ⋆_  : ∀ {A} → A ∈ Γ → Term Γ A
    Λ   : ∀ {A B} → Term (A ∷ Γ) B → Term Γ (A →' B)
    _∙_ : ∀ {A B} → Term Γ (A →' B) → Term Γ A → Term Γ B

  -- Context (free variables) manipulations:

  -- Add free variable
  _↓w⋯_ : ∀ {Γ Δ} γ → Γ ⊆ Δ → Γ ⊆ (γ ∷ Δ)
  _↓w⋯_ γ f n = there (f n)

  -- Remove free variable
  _↑w⋯_ : ∀ {Γ Δ} γ → (γ ∷ Γ) ⊆ Δ → Γ ⊆ Δ
  _↑w⋯_ γ f n = f (there n)

  -- Skip free variable
  _∷w⋯_ : ∀ {Γ Δ} γ → Γ ⊆ Δ → (γ ∷ Γ) ⊆ (γ ∷ Δ)
  _∷w⋯_ γ f (here refl) = here refl
  _∷w⋯_ γ f (there n) = there (f n)

  -- Skip a bunch of free variables
  _w⋯_ : ∀ {Γ Δ} γs → Γ ⊆ Δ → (γs ++ Γ) ⊆ (γs ++ Δ)
  []     w⋯ f = f
  γ ∷ γs w⋯ f = γ ∷w⋯ γs w⋯ f

  -- Example:
  -- Adding a new free variable σ after free variables γs:
  --   γs w⋯ σ ∷w⋯ id
  -- .

  -- Weakening: apply context manipulation to a λ-term
  wk : ∀ {Γ Δ τ} → Term Γ τ → (Γ ⊆ Δ) → Term Δ τ
  wk (⋆ x) f = ⋆ (f x)
  wk (Λ {A} M) f = Λ (wk M (A ∷w⋯ f))
  wk (M ∙ N) f = wk M f ∙ wk N f

  -- Parallel substitution
  -- Change a term with context Γ to a term with
  -- context Δ by
  data Sub : (Γ Δ : Ctx) → Set where
    [[]]     : Sub [] []
    [_↦_]∷⋯_ : ∀ {Γ Δ} → ∀ γ → Term Δ γ → Sub Γ Δ → Sub (γ ∷ Γ) Δ -- substituting
    _∷⋯_     : ∀ {Γ Δ} → ∀ γ → Sub Γ Δ  → Sub (γ ∷ Γ) (γ ∷ Δ)     -- or skipping
  -- free variables.
  -- Sub is a function with finite domain, represented by a list of respective
  -- codomain elements.

  -- Skip a bunch of variables
  _⋯_      : ∀ {Γ Δ} γs → Sub Γ Δ → Sub (γs ++ Γ) (γs ++ Δ)
  [] ⋯ ss = ss
  (γ ∷ γs) ⋯ ss = γ ∷⋯ (γs ⋯ ss)

  -- Identity substitution
  [id] : ∀ {Γ} → Sub Γ Γ
  [id] {[]} = [[]]
  [id] {γ ∷ Γ} = γ ∷⋯ [id]

  -- Syntax sugar
  [_↦_] : ∀ {Γ} γ → Term Γ γ → Sub (γ ∷ Γ) Γ
  [ γ ↦ M ] = [ γ ↦ M ]∷⋯ [id]

  infixr 4 _∷⋯_ _⋯_
  infixr 4 _∷w⋯_ _↓w⋯_ _w⋯_

  -- Extract a term from a substitution by its number in a context
  _!_ : ∀ {Γ Δ τ} → Sub Γ Δ → τ ∈ Γ → Term Δ τ
  [[]]             ! ()
  ([ γ ↦ x ]∷⋯ ss) ! here refl = x
  ([ γ ↦ x ]∷⋯ ss) ! there n   = ss ! n
  (γ        ∷⋯ ss) ! here refl = ⋆ here refl
  (γ        ∷⋯ ss) ! there n   = wk (ss ! n) (γ ↓w⋯ id)

  -- Apply substitution to a term
  sub : ∀ {Γ Δ τ} → Term Γ τ → Sub Γ Δ → Term Δ τ
  sub (⋆ x) ss = ss ! x
  sub (Λ {A} M) ss = Λ (sub M (A ∷⋯ ss))
  sub (M ∙ N) ss = sub M ss ∙ sub N ss

  -- Weakening commutes with substitution
  -- This is specific for the current term representation.
  lemma-wk : ∀ {Γ Δ τ γ}
           → (f : Γ ⊆ Δ)
           → (M : Term (γ ∷ Γ) τ) (s : Term Γ γ)
           → sub (wk M (γ ∷w⋯ f)) [ γ ↦ wk s f ]
           ≡ wk (sub M [ γ ↦ s ]) f
  lemma-wk f M s = {!!}

-- Λ   : ∀ {A B} → Term (A ∷ Γ) B → Term Γ (A →' B)
  lemma-sub₀ : ∀ {Γ Δ τ σ}
               → (ts : List Type)
               → (M : Term (ts ++ (σ ∷ Γ)) τ) (N : Term Γ σ)
               → (ss : Sub Γ Δ)
               → sub (sub M (ts ⋯ σ ∷⋯ ss)) (ts ⋯ [ σ ↦ sub N ss ])
               ≡ sub (sub M (ts ⋯ [ σ ↦ N ])) (ts ⋯ ss)
  lemma-sub₀ ts M N ss = {!!}

  -- The Substitution Lemma: substitution commutes with itself
  -- This is general.
  lemma-sub : ∀ {Γ Δ σ τ}
              → (M : Term (σ ∷ Γ) τ) (N : Term Γ σ)
              → (ss : Sub Γ Δ)
              → sub (sub M (σ ∷⋯ ss)) [ σ ↦ sub N ss ]
              ≡ sub (sub M [ σ ↦ N ]) ss
  lemma-sub (⋆ pos) N ss = lemma-sub₀ [] (⋆ pos) N ss
  lemma-sub (Λ {A} M) N ss = cong Λ (lemma-sub₀ {_} {_} {_} {_} (A ∷ []) M N ss)
  lemma-sub (M ∙ M₁) N ss rewrite lemma-sub M N ss | lemma-sub M₁ N ss = refl

  -- β-reduction
  data _→β_ {Γ} : ∀ {τ} → Term Γ τ → Term Γ τ → Set where
    reduce : ∀ {τ γ} {M : Term (γ ∷ Γ) τ} {N : Term Γ γ} → ((Λ M) ∙ N) →β (sub M [ γ ↦ N ])
    under  : ∀ {τ γ} {M N : Term (γ ∷ Γ) τ} → M →β N → (Λ M) →β (Λ N)
    left   : ∀ {τ σ} {M N : Term Γ (σ →' τ)} {L : Term Γ σ} → M →β N → (M ∙ L) →β (N ∙ L)
    right  : ∀ {τ σ} {M N : Term Γ σ} {L : Term Γ (σ →' τ)} → M →β N → (L ∙ M) →β (L ∙ N)

  -- β-reduction sequence
  _→β✴_ : ∀ {Γ τ} → Term Γ τ → Term Γ τ → Set
  _→β✴_ = _→β_ ✴

  -- We want to prove that →β✴ is confluent, the problem is →β is not confluent
  -- (no free GeneralizedCR proof).
  -- That's why we define

  -- Parallel β-reduction
  data _⇉β_ {Γ} : ∀ {τ} → Term Γ τ → Term Γ τ → Set where
    parsame   : ∀ {τ} → {M : Term Γ τ} → M ⇉β M
    parreduce : ∀ {τ γ} {M M' : Term (γ ∷ Γ) τ} {N N' : Term Γ γ} → M ⇉β M' → N ⇉β N' → ((Λ M) ∙ N) ⇉β (sub M' [ γ ↦ N' ])
    parunder  : ∀ {τ γ} {M N : Term (γ ∷ Γ) τ} → M ⇉β N → (Λ M) ⇉β (Λ N)
    parapp    : ∀ {τ σ} {M M' : Term Γ (σ →' τ)} {N N' : Term Γ σ} → M ⇉β M' → N ⇉β N' → (M ∙ N) ⇉β (M' ∙ N')

  -- Parallel β-reduction sequence
  _⇉β✴_ : ∀ {Γ τ} → Term Γ τ → Term Γ τ → Set
  _⇉β✴_ = _⇉β_ ✴

  -- with ⇉β being confluent (GeneralizedCR!), and ⇉β✴ being the same thing as →β✴.
  -- The rest of the module proves this.

  module TechnicalReductionLemmas where
    -- Useful transformations:
    →β-≡ : ∀ {Γ τ} → {M N M' N' : Term Γ τ} → M ≡ M' → N ≡ N' → M →β N → M' →β N'
    →β-≡ refl refl red = red

    ⇉β-≡ : ∀ {Γ τ} → {M N M' N' : Term Γ τ} → M ≡ M' → N ≡ N' → M ⇉β N → M' ⇉β N'
    ⇉β-≡ refl refl red = red

    -- ?

    {- TechnicalReductionLemmas end -}

  wk-test : ∀ {Γ Δ τ ts} → {x y : Term (ts ++ Γ) τ}
          → (f : (ts ++ Γ) ⊆ (ts ++ Δ))
          → x →β y → wk x f →β wk y f
  wk-test {Γ} {Δ} {τ} {ts} {⋆ n} {⋆ x} f ()
  wk-test {Γ} {Δ} {A →' B} {ts} {⋆ n} {Λ x} f ()
  wk-test {Γ} {Δ} {τ} {ts} {⋆ n} {x ∙ y} f ()
  wk-test {Γ} {Δ} {A →' B} {ts} {Λ n} {⋆ y} f ()
  wk-test {Γ} {Δ} {A →' B} {ts} {Λ n} {Λ y} f (under bs) = under (wk-test {Γ} {ts = A ∷ ts} {_} {_} _ bs)
  wk-test {Γ} {Δ} {A →' B} {ts} {Λ n} {x ∙ y} f ()

  wk-test {Γ} {Δ} {τ} {ts} {m ∙ n} {⋆ x} f bs = {!!}
  wk-test {Γ} {Δ} {(A₁ →' B)} {ts} {m ∙ n} {Λ x} f bs = {!!}
  wk-test {Γ} {Δ} {τ} {ts} {m ∙ n} {x ∙ y} f bs = {!!}


  wk-test₂ : ∀ {Γ Δ τ ts} → {x y : Term (ts ++ Γ) τ}
          → (f : (ts ++ Γ) ⊆ (ts ++ Δ))
          → x →β✴ y → wk x f →β✴ wk y f
  wk-test₂ {Γ} {Δ} {τ} {ts} {⋆ n} {⋆ x} f xy = {!!}
  wk-test₂ {Γ} {Δ} {(A →' B)} {ts} {⋆ n} {Λ x} f xy = {!!}
  wk-test₂ {Γ} {Δ} {τ} {ts} {⋆ n} {x ∙ y} f xy = {!!}
  wk-test₂ {Γ} {Δ} {(A →' B)} {ts} {Λ n} {⋆ x} f xy = {!!}
  wk-test₂ {Γ} {Δ} {(A →' B)} {ts} {Λ n} {Λ x} f xy = {!!}
  wk-test₂ {Γ} {Δ} {(A →' B)} {ts} {Λ n} {x ∙ y} f xy = {!!}
  wk-test₂ {Γ} {Δ} {τ} {ts} {n ∙ m} {⋆ x} f xy = {!!}
  wk-test₂ {Γ} {Δ} {(A →' B)} {ts} {n ∙ m} {Λ x} f xy = {!!}
  wk-test₂ {Γ} {Δ} {τ} {ts} {n ∙ m} {x ∙ y} f xy = {!!}

  →β-sub₁ : ∀ {Γ τ γ}
          → (ts : List Type)
          → {M : Term (ts ++ (γ ∷ Γ)) τ}
          → {N N' : Term Γ γ} → N →β✴ N'
          → sub M (ts ⋯ [ γ ↦ N ]) →β✴ sub M (ts ⋯ [ γ ↦ N' ])
  →β-sub₁ [] {⋆ here refl} nn = nn
  →β-sub₁ [] {⋆ there pa} nn = ε
  →β-sub₁ (t ∷ ts) {⋆ here refl} nn = ε
  →β-sub₁ {Γ} {τ} {γ} (t ∷ ts) {⋆ there pa} nn = {!!} -- map✴ (λ x → wk x (_ ↓w⋯ id)) (λ {x} {y} xy → wk-test {Γ} {Γ} {τ} {{! ts ++ Γ!}} {x} {y} (λ n → there n) xy) (→β-sub₁ ts {⋆ pa} nn)
  →β-sub₁ {Γ} {(A →' B)} {γ} ts {Λ M} nn = map✴ Λ under (→β-sub₁ (A ∷ ts) {M} nn)
  →β-sub₁ {Γ} {τ} {γ} ts {M ∙ M₁} nn = map✴ (λ z → z ∙ _) left (→β-sub₁ ts {M = M} nn) ++✴ map✴ (_∙_ _) right (→β-sub₁ ts {M = M₁} nn)

  →β-sub₂ : ∀ {Γ τ γ}
          → (ts : List Type)
          → {M M' : Term (ts ++ (γ ∷ Γ)) τ}
          → {N : Term Γ γ}
          → M →β M'
          → sub M (ts ⋯ [ γ ↦ N ]) →β sub M' (ts ⋯ [ γ ↦ N ])
  →β-sub₂ mm = {!!}

  -- Substitution is substitutive for →β✴
  →β✴-sub : ∀ {Γ τ γ}
          → {M M' : Term (γ ∷ Γ) τ} → M →β✴ M'
          → {N N' : Term Γ γ} → N →β✴ N'
          → sub M [ γ ↦ N ] →β✴ sub M' [ γ ↦ N' ]
  →β✴-sub {Γ} {γ = γ} {M = M} {M'} ms {N} ns = {!!}

  -- Substitution is substitutive for ⇉β
  ⇉β-sub : ∀ {Γ τ γ}
         → {M M' : Term (γ ∷ Γ) τ} → M ⇉β M'
         → {N N' : Term Γ γ} → N ⇉β N'
         → sub M [ γ ↦ N ] ⇉β sub M' [ γ ↦ N' ]
  ⇉β-sub ms ns = {! !}

  -- ⇉β is confluent
  ⇉β-confluent : ∀ {Γ τ} → Confluent {Term Γ τ} _⇉β_
  ⇉β-confluent parsame parsame = _ , parsame , parsame
  ⇉β-confluent parsame (parreduce m n) = _ , parreduce m n , parsame
  ⇉β-confluent parsame (parunder mn) = _ , parunder mn , parsame
  ⇉β-confluent parsame (parapp m n) = _ , parapp m n , parsame
  ⇉β-confluent (parreduce m n) parsame = _ , parsame , parreduce m n

  ⇉β-confluent (parreduce {τ} {γ} m₁ n₁) (parreduce .{τ} .{γ} m₂ n₂) with ⇉β-confluent m₁ m₂ | ⇉β-confluent n₁ n₂
  ⇉β-confluent (parreduce {τ} {γ} {M} {M'} {N} {N'} m₁ n₁) (parreduce .{τ} .{γ} .{M} {M''} .{N} {N''} m₂ n₂) | dm , md₁ , md₂ | dn , nd₁ , nd₂ with ⇉β-sub {_} {_} {_} {M'} {dm} md₁ {N'} {dn} nd₁ | ⇉β-sub {_} {_} {_} {M''} {dm} md₂ {N''} {dn} nd₂
  ... | s1 | s2 = sub dm [ γ ↦ dn ] , s1 , s2

  ⇉β-confluent (parreduce {_} {γ} m₁ n₁) (parapp parsame n₂) with ⇉β-confluent n₁ n₂
  ⇉β-confluent (parreduce {_} {γ} {_} {M'} m₁ n₁) (parapp parsame n₂) | dn , nd₁ , nd₂ = sub M' [ γ ↦ dn ] , ⇉β-sub {_} {_} {_} {M'} parsame nd₁ , parreduce m₁ nd₂

  ⇉β-confluent (parreduce {_} {γ} m₁ n₁) (parapp (parunder m₂) n₂) with ⇉β-confluent m₁ m₂ | ⇉β-confluent n₁ n₂
  ⇉β-confluent (parreduce {_} {γ} m₁ n₁) (parapp (parunder m₂) n₂) | dm , md₁ , md₂ | dn , nd₁ , nd₂ with ⇉β-sub md₁ nd₁ | ⇉β-sub md₂ nd₂ 
  ... | s1 | s2 = sub dm [ γ ↦ dn ] , s1 , parreduce md₂ nd₂

  ⇉β-confluent (parunder m) parsame = _ , parsame , parunder m
  ⇉β-confluent (parunder mn₁) (parunder mn₂) with ⇉β-confluent mn₁ mn₂
  ⇉β-confluent (parunder mn₁) (parunder mn₂) | d , nd₁ , nd₂ = Λ d , parunder nd₁ , parunder nd₂
  ⇉β-confluent (parapp m n) parsame = _ , parsame , parapp m n

  ⇉β-confluent (parapp parsame n₁) (parreduce {_} {γ} m₁ n₂) with ⇉β-confluent n₁ n₂
  ⇉β-confluent (parapp parsame n₁) (parreduce {_} {γ} {_} {M'} m₁ n₂) | dn , nd₁ , nd₂ = sub M' [ γ ↦ dn ] , parreduce m₁ nd₁ , ⇉β-sub {_} {_} {_} {M'} parsame nd₂

  ⇉β-confluent (parapp (parunder m₁) n₁) (parreduce {_} {γ} m₂ n₂) with ⇉β-confluent m₁ m₂ | ⇉β-confluent n₁ n₂
  ⇉β-confluent (parapp (parunder m₁) n₁) (parreduce {_} {γ} m₂ n₂) | dm , md₁ , md₂ | dn , nd₁ , nd₂ with ⇉β-sub md₁ nd₁ | ⇉β-sub md₂ nd₂
  ... | s1 | s2 = sub dm [ γ ↦ dn ] , parreduce md₁ nd₁ , s2

  ⇉β-confluent (parapp m₁ n₁) (parapp m₂ n₂) with ⇉β-confluent m₁ m₂ | ⇉β-confluent n₁ n₂
  ⇉β-confluent (parapp m₁ n₁) (parapp m₂ n₂) | dm , (md₁ , md₂) | dn , (nd₁ , nd₂) = dm ∙ dn , parapp md₁ nd₁ , parapp md₂ nd₂

  -- Transformations between →β✴ and ⇉β✴ show that they are equivalent:

  →β-⇉β : ∀ {Γ τ} {M N : Term Γ τ} → M →β N → M ⇉β N
  →β-⇉β reduce = parreduce parsame parsame
  →β-⇉β (under r) = parunder (→β-⇉β r)
  →β-⇉β (left r) = parapp (→β-⇉β r) parsame
  →β-⇉β (right r) = parapp parsame (→β-⇉β r)

  →β-⇉β✴ : ∀ {Γ τ} {M N : Term Γ τ} → M →β✴ N → M ⇉β✴ N
  →β-⇉β✴ = map✴ id →β-⇉β

  ⇉β-→β : ∀ {Γ τ} {M N : Term Γ τ} → M ⇉β N → M →β✴ N
  ⇉β-→β {M = M} .{M} parsame = ε
  ⇉β-→β (parreduce {M = M} {M'} {N} {N'} m n) with ⇉β-→β m | ⇉β-→β n
  ⇉β-→β (parreduce {M = M} .{M} m n) | ε | ε = reduce ∷✴ ε
  ⇉β-→β (parreduce {M = M} .{M} m n) | ε | ns = reduce ∷✴ →β✴-sub {M = M} {M} ε ns
  ⇉β-→β (parreduce {N = N} .{N} m n) | ms | ε = reduce ∷✴ →β✴-sub ms {N = N} {N} ε
  ⇉β-→β (parreduce m n) | ms | ns = reduce ∷✴ →β✴-sub ms ns

  ⇉β-→β (parunder mn) = map✴ Λ under (⇉β-→β mn)

  ⇉β-→β (parapp m n) with ⇉β-→β m | ⇉β-→β n
  ⇉β-→β (parapp m n) | ms | ns = map✴ (λ z → z ∙ _) (λ {x} {y} → left) ms ++✴ map✴ (_∙_ _) (λ {x} {y} → right) ns

  ⇉β-→β✴ : ∀ {Γ τ} {M N : Term Γ τ} → M ⇉β✴ N → M →β✴ N
  ⇉β-→β✴ = concat✴ ∘ map✴ id ⇉β-→β

  -- →β✴ is confluent
  →β✴-confluent : ∀ {Γ τ} → Confluent (_→β✴_ {Γ = Γ} {τ = τ})
  →β✴-confluent ra rx with CR ⇉β-confluent (→β-⇉β✴ ra) (→β-⇉β✴ rx)
  ... | L , (rb , ry) = L , (⇉β-→β✴ rb , ⇉β-→β✴ ry)

