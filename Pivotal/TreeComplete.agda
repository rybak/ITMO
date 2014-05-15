module TreeComplete where

data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

data Tree (A : Set) : Set where
  leaf : Tree A
  node : A → Tree A → Tree A → Tree A
  
data Path {A : Set} : Tree A → Set where
  -- empty : Path leaf
  here : ∀ {p l r} → Path (node p l r)
  left : ∀ {p l r} → Path l → Path (node p l r)
  right : ∀ {p l r} → Path r → Path (node p l r)
 
data Full {A : Set} : ℕ → Tree A → Set where
  none : Full zero leaf
  fork : ∀ {p n l r} → Full n l → Full n r
    → Full (succ n) (node p l r)
  
data Filled {A : Set} : ℕ → Tree A → Set where
  empty : Filled zero leaf
  left-filled : ∀ {p n l r} → Filled (succ n) l → Full n r
    → Filled (succ (succ n)) (node p l r)
  left-full : ∀ {p n l r} → Full n l → Filled n r
    → Filled (succ n) (node p l r)
    
