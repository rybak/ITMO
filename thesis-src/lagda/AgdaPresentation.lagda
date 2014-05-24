\begin{code}
module AgdaPresentation where
data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ
\end{code} \AgdaHide{\begin{code}
{-# BUILTIN NATURAL ℕ #-}
{-# BUILTIN ZERO zero #-}
{-# BUILTIN SUC succ #-}
\end{code}}
\begin{code}
_+_ : ℕ → ℕ → ℕ
zero + b = b
succ a + b = succ (a + b)

data Vec (A : Set) : ℕ → Set where
  nil  : Vec A zero
  cons : ∀ {n} → A → Vec A n → Vec A (succ n)

head : ∀ {A} {n} → Vec A (succ n) → A
head (cons a as) = a
\end{code}

