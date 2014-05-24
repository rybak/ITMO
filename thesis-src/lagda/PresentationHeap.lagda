\AgdaHide{

\begin{code}
module PresentationHeap where
open import AgdaDescription
\end{code}
}

\begin{code}
data TreeState : Set where
  full almost : TreeState

data Tree : (h : ℕ) → TreeState → Set where
  et : Tree zero full -- Пустое дерево
  nf : ∀ {n} → (a : Tree n full) → (b : Tree n full)
      → Tree (succ n) full -- Полное дерево
  nd : ∀ {n} → (a : Tree (succ n) full) → (b : Tree n full)
      → Tree (succ (succ n)) almost -- Полные поддеревья разной высоты
  nl : ∀ {n} → (a : Tree (succ n) almost) → (b : Tree n full)
      → Tree (succ (succ n)) almost -- Правое поддерево — полное
  nr : ∀ {n} → (a : Tree n full) → (b : Tree n almost)
      → Tree (succ n) almost -- Левое поддерево — полное

\end{code}
