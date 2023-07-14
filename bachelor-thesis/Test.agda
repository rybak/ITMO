module Test where

record ⊤ : Set where
data ⊤₁ : Set where
  tt : ⊤₁

data ⊥ : Set where

data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

div2 : ℕ → ℕ
div2 zero = zero
div2 (succ zero) = zero
div2 (succ (succ n)) = succ (div2 n)

even : ℕ → Set
even zero = ⊤
even (succ zero) = ⊥
even (succ (succ n)) = even n

data Even : ℕ → Set where
  E₀ : Even zero
  E : (n : ℕ) → Even n → Even (succ (succ n))

div2e : (n : ℕ) → even n → ℕ
div2e zero p = zero
div2e (succ zero) ()
div2e (succ (succ y)) p = y

div2E : (a : ℕ) → Even a → ℕ
div2E zero E₀ = zero
div2E (succ zero) ()
div2E (succ (succ y)) (E .y y') = succ (div2E y y')

data _≤_ : ℕ → ℕ → Set where
  z≤n : ∀ n → zero ≤ n
  s≤s : ∀ {n m} → n ≤ m → succ n ≤ succ m

data Bool : Set where
  true false : Bool

if_then_else_ : {A : Set} → Bool → A → A → A
if true then a else _ = a
if false then _ else b = b

sub : (n m : ℕ) → m ≤ n → ℕ
sub n .zero (z≤n .n) = n
sub .(succ m) .(succ n) (s≤s {n} {m} y) = sub m n y

data _≡_ {A : Set} (a : A) : A → Set where
  refl : a ≡ a

_+_ : ℕ → ℕ → ℕ
zero + n = n
succ n + m = succ (n + m)

infix 40 _+_
infix 6 _≡_

cong : {A B : Set}{x y : A}(f : A → B) → x ≡ y → f x ≡ f y
cong f refl = refl

≡-sym : {A : Set}{a b : A} → a ≡ b → b ≡ a
≡-sym refl = refl

zero-lem : ∀ n → n + zero ≡ zero + n
zero-lem zero = refl
zero-lem (succ y) = cong succ (zero-lem y)

assoc+ : ∀ a b c → (a + b) + c ≡ a + (b + c)
assoc+ zero b c = refl
assoc+ (succ a) b c = cong succ (assoc+ a b c)

succ-lem : ∀ n m → n + (succ m) ≡ succ (n + m)
succ-lem zero m = refl
succ-lem (succ n) m = cong succ (succ-lem n m)

_~_ : {A : Set} {a b c : A} → a ≡ b → b ≡ c → a ≡ c
refl ~ refl = refl

comm+₁ : ∀ a b → a + b ≡ b + a
comm+₁ zero b = ≡-sym (zero-lem b)
comm+₁ (succ a) b = ≡-sym (cong succ (comm+₁ b a)) ~ ≡-sym (succ-lem b a)

comm+ : ∀ a b → a + b ≡ b + a
comm+ zero m = ≡-sym (zero-lem m)
comm+ (succ n) m with n + m | comm+ n m
... | .(m + n) | refl with m + (succ n) | succ-lem m n
... | ._ | refl = refl

data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A

--------∀-----------∃
data Vec (A : Set) : ℕ → Set where
  []  : Vec A zero
  _∷_ : ∀ {n} → A → Vec A n → Vec A (succ n)

head : ∀ {A n} → Vec A (succ n) → A
head (y ∷ y') = y

tail : ∀ {A n} → Vec A (succ n) → Vec A n
tail (y ∷ y') = y'

last : ∀ {A n} → Vec A (succ n) → A
last (y ∷ []) = y
last (y ∷ (y' ∷ y0)) = last (y' ∷ y0)

filter : ∀ {A} → (A → Bool) → List A → List A
filter p [] = []
filter p (y ∷ y') with p y
... | true = y ∷ filter p y'
... | false = filter p y'

data _⊂_ {A : Set} : List A → List A → Set where
  empty : [] ⊂ []
  skip  : ∀ y {xs ys} → xs ⊂ ys → xs ⊂ (y ∷ ys)
  keep  : ∀ y {xs ys} → xs ⊂ ys → (y ∷ xs) ⊂ (y ∷ ys)

filter-lemma : ∀ {A} → (p : A → Bool) → (ls : List A) → filter p ls ⊂ ls
filter-lemma p [] = empty
filter-lemma p (x ∷ ls) with (p x)
filter-lemma p (x ∷ ls) | true = keep x (filter-lemma p ls)
filter-lemma p (x ∷ ls) | false = skip x (filter-lemma p ls)

¬ : Set → Set
¬ p = p → ⊥

bot-elim : ∀ {P : Set} → ⊥ → P
bot-elim ()

contradiction : ∀ {A B : Set} → A → (A → ⊥) → B
contradiction a na = bot-elim (na a)

data Dec (P : Set) : Set where
  yes : P → Dec P
  no  : ¬ P → Dec P

contr : ∀ {A B : Set} → (A → B) → (¬ B → ¬ A)
contr p ¬b a = ¬b (p a)

gfilter : ∀ {A} {P : A → Set} → ((a : A) → Dec (P a)) → List A → List A
gfilter p [] = []
gfilter p (y ∷ y') with p y
gfilter p (y ∷ y0) | yes y1 = y ∷ gfilter p y0
gfilter p (y ∷ y0) | no y1 = gfilter p y0

data _∈_ {A : Set}(a : A) : List A → Set where
  here  : ∀ {ls} → a ∈ (a ∷ ls)
  there : ∀ x {ls} → a ∈ ls → a ∈ (x ∷ ls)

record Σ (A : Set) (B : A → Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B fst

_∧_ : (A B : Set) → Set
A ∧ B = Σ A (λ x → B)

