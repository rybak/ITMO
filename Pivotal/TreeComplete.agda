module TreeComplete where

data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

data Tree : Set where
  leaf : Tree
  fork : Tree → Tree → Tree

data Path : Tree → Set where
  deadend : Path leaf
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

