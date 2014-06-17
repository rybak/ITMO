\section{Agda}
\begin{frame}
    \frametitle{Agda}
\begin{code}
module AgdaPresentation where
data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ
_+_ : ℕ → ℕ → ℕ
zero + b = b
succ a + b = succ (a + b)
\end{code}
\end{frame}
\begin{frame}
    \frametitle{Vec}
\begin{code}
data Vec (A : Set) : ℕ → Set where
  nil  : Vec A zero
  cons : ∀ {n} → A → Vec A n
    → Vec A (succ n)

head : ∀ {A} {n} → Vec A (succ n) → A
head (cons a as) = a

\end{code}
\end{frame}
