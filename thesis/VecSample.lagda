\begin{code}
module VecSample where
data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ
data Vec A : ℕ → Set where
  nil  : Vec A zero
  cons : ∀ {n} → A → Vec A n → Vec A (succ n)
\end{code}

Такое определение позволяет нам описать функцию $ \F{head} $ для такого списка, которая не может бросить исключение:
\begin{code}
head : ∀ {A} {n} → Vec A (succ n) → A
\end{code}
У аргумента функции $ \F{head} $ тип $ \D{Vec}\,A\,(\DC{succ}\,n) $, то есть вектор, в котором есть хотя бы один элемент.
Это позволяет произвести сопоставление с образцом только по конструктору $ \DC{cons} $:
\begin{code}
head (cons a as) = a
\end{code}
