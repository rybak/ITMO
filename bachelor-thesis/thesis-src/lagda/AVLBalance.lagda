\AgdaHide{
\begin{code}
module AVLBalance where
  open import AgdaDescription
  infix 4 _∼_
  testˡ : ℕ
  testˡ = succ zero
\end{code}}

Если $m \sim n$, то разница между $m$ и $n$ не больше чем один:
\begin{code}
  data _∼_ : ℕ → ℕ → Set where
    ∼+ : ∀ {n} →     n ∼ 1 + n
    ∼0 : ∀ {n} →     n ∼ n
    ∼- : ∀ {n} → 1 + n ∼ n
\end{code}
