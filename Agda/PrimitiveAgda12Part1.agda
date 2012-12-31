-- Date: December 2012
-- Author: Jan Malakhovski
-- WWW: http://twitter.com/oxij

-- Introduction to Agda. Early (but usable) draft
-- for Functional Programming and Proof Checking course at NRU ITMO.

module PrimitiveAgda12Part1 where
    


data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ



data ⊥ : Set where
    


⊥-elim : {A : Set} → ⊥ → A
⊥-elim ()
    


⊥-elim‵ : {A : Set} → ⊥ → A
⊥-elim‵ x = ⊥-elim x
    


record ⊤ : Set where
  constructor tt

test⊤ : ⊤
test⊤ = tt
    
data Bool : Set where
  true false : Bool
                   

not : Bool → Bool
not true  = false
not false = true
    
not‵ : Bool → Bool
not‵ true  = false
not‵ fals  = true
    
⊥-elim‵‵ : {A : Set} → ⊥ → A
⊥-elim‵‵ ∀x:⊥→-- = ⊥-elim ∀x:⊥→--
    
    
even : ℕ → Set
even zero = ⊤
even (succ zero) = ⊥
even (succ (succ n)) = even n
    
div2e : (n : ℕ) → even n → ℕ
div2e zero p = zero
div2e (succ zero) ()
div2e (succ (succ y)) p = succ (div2e y p)
    
data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A
    
data Even : ℕ → Set where
  ezero  : Even zero
  e2succ : {n : ℕ} → Even n → Even (succ (succ n))
    
evenTest1 : Even (succ (succ zero))
evenTest1 = e2succ {n = _} ezero
    
    
listTest1 : List ℕ
listTest1 = [] {ℕ}
    
listTest2 : List ℕ
listTest2 = [] {A = ℕ}
    
evenTest2 : ∀ {n} → Even n → ℕ
evenTest2 ezero      = zero
evenTest2 (e2succ {n = n} e) = n
    
div2E : (n : ℕ) → Even n → ℕ
div2E zero ezero = zero
div2E (succ zero) ()
div2E (succ (succ n)) (e2succ stilleven) = succ (div2E n stilleven)
    
data Vec (A : Set) : ℕ → Set where
  []  : Vec A zero
  _∷_ : ∀ {n} → A → Vec A n → Vec A (succ n)
    
head : ∀ {A n} → Vec A (succ n) → A
head (_∷_ a as) = a
    
if_then_else_ : {A : Set} → Bool → A → A → A
if true then a else _ = a
if false then _ else b = b
    

_=ℕ?_ : ℕ → ℕ → Bool
zero   =ℕ? zero   = true
zero   =ℕ? succ m = false
succ m =ℕ? zero   = false
succ n =ℕ? succ m = n =ℕ? m


infix 20 _+_
_+_ : ℕ → ℕ → ℕ
zero   + n = n
succ n + m = succ (n + m)
    
ifthenelseTest : ℕ
ifthenelseTest = if (zero + succ zero) =ℕ? zero
  then zero
  else succ (succ zero)
    
head‵ : ∀ {A n} → Vec A (succ n) → A
head‵ (a ∷ as) = a
    
_++_ : ∀ {A n m} → Vec A n → Vec A m → Vec A (n + m)
[]       ++ bs = bs
(a ∷ as) ++ bs = a ∷ (as ++ bs)
    
infix 20 _-_
_-_ : ℕ → ℕ → ℕ
zero   - _      = zero
succ n - zero   = succ n
succ n - succ m = n - m
    
data _≤_ : ℕ → ℕ → Set where
  z≤n : ∀ {n}           → zero ≤ n
  s≤s : ∀ {n m} → n ≤ m → succ n ≤ succ m
    
sub : (n m : ℕ) → m ≤ n → ℕ
sub n zero (z≤n .{n}) = n
sub .(succ n) .(succ m) (s≤s {m} {n} y) = sub n m y
    
sub₁ : (n m : ℕ) → m ≤ n → ℕ
sub₁ n .zero (z≤n .{n}) = n
sub₁ .(succ n) .(succ m) (s≤s {m} {n} y) = sub₁ n m y
    
sub‵ : (n m : ℕ) → m ≤ n → ℕ
sub‵ n zero (z≤n .{n}) = n
sub‵ zero (succ _) ()
sub‵ (succ n) (succ m) (s≤s .{m} .{n} y) = sub‵ n m y
    
sub₃ : (n m : ℕ) → m ≤ n → ℕ
sub₃ n zero _ = n
sub₃ zero (succ _) ()
sub₃ (succ n) (succ m) (s≤s .{m} .{n} y) = sub₃ n m y
    
sub₁‵ : (n m : ℕ) → m ≤ n → ℕ
sub₁‵ n zero (z≤n .{n}) = n
sub₁‵ zero (succ _) ()
sub₁‵ (succ .n) (succ .m) (s≤s {m} {n} y) = sub₁‵ n m y
    

infix 10 _≡_
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x
    

sym : {A : Set}{a b : A} → a ≡ b → b ≡ a
sym refl = refl


trans : {A : Set}{a b c : A} → a ≡ b → b ≡ c → a ≡ c
trans refl refl = refl


infixr 5 _~_
_~_ : {A : Set}{a b c : A} → a ≡ b → b ≡ c → a ≡ c
_~_ = trans


cong : {A B : Set} {a b : A} → (f : A → B) → a ≡ b → f a ≡ f b
cong f refl = refl
    
sym‵ : {A : Set}{a b : A} → a ≡ b → b ≡ a
sym‵ {A} .{b} {b} refl = refl
    
+-assoc : ∀ a b c → (a + b) + c ≡ a + (b + c)
+-assoc zero b c = refl
+-assoc (succ a) b c = cong succ (+-assoc a b c)
    
lemma-+zero : ∀ a → a ≡ a + zero
lemma-+zero zero = refl
lemma-+zero (succ a) = cong succ (lemma-+zero a)

lemma-+succ : ∀ a b → succ a + b ≡ a + succ b
lemma-+succ zero b = refl
lemma-+succ (succ a) b = cong succ (lemma-+succ a b)
    
+-comm : ∀ a b → a + b ≡ b + a
+-comm zero b = lemma-+zero b
+-comm (succ a) b = cong succ (+-comm a b) ~ lemma-+succ b a
    
    
infix 30 _*_
_*_ : ℕ → ℕ → ℕ
zero * m = zero
(succ n) * m = m + n * m
    

dist : ∀ a b c → (a + b) * c ≡ a * c + b * c
dist zero b c = refl

dist (succ a) b c = cong (λ x → c + x) (dist a b c) ~ sym (+-assoc c (a * c) (b * c))
    
*-assoc : ∀ a b c → (a * b) * c ≡ a * (b * c)
*-assoc zero b c = refl
*-assoc (succ a) b c = dist b (a * b) c ~ cong (_+_ (b * c)) (*-assoc a b c)

lemma-*zero : ∀ a → zero ≡ a * zero
lemma-*zero zero = refl
lemma-*zero (succ a) = lemma-*zero a

lemma-+swap : ∀ a b c → a + (b + c) ≡ b + (a + c)
lemma-+swap a b c = sym (+-assoc a b c) ~ cong (λ x → x + c) (+-comm a b) ~ +-assoc b a c

lemma-*succ : ∀ a b → a + a * b ≡ a * succ b 
lemma-*succ zero b = refl
lemma-*succ (succ a) b = cong succ (lemma-+swap a b (a * b) ~ cong (_+_ b) (lemma-*succ a b))

*-comm : ∀ a b → a * b ≡ b * a
*-comm a zero = sym (lemma-*zero a)
*-comm a (succ b) = sym (cong (_+_ a) (sym (*-comm a b)) ~ lemma-*succ a b) 
    

_∘_ : {A : Set} {B : A → Set} {C : {x : A} → B x → Set}
    → (f : {x : A} → (y : B x) → C y)
    → (g : (x : A) → B x)
    → ((x : A) → C (g x))
f ∘ g = λ x → f (g x)
    

¬ : Set → Set
¬ P = P → ⊥
    
contradiction : {A B : Set} → A → ¬ A → B
contradiction a ¬a = ⊥-elim (¬a a)
    
    
contra : {A B : Set} → (A → B) → (¬ B → ¬ A)

contra f g a = (g ∘ f) a

¬³-red : {A : Set} → ¬ (¬ (¬ A)) → ¬ A

¬³-red f a = f (λ b → b a)

    
