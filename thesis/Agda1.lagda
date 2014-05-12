Пример:
\newline

\begin{code}

module Agda1 where

data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero + b = b
succ a + b = succ (a + b)
\end{code}
