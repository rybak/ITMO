В Agda есть поддержка модулей:
\begin{code}
module AgdaDescription where
\end{code} В коде на Agda широко используются символы Unicode.
Тип натуральных чисел — \D{ℕ}.
\begin{code}
data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ
\end{code} \AgdaHide{
\begin{code}
{-# BUILTIN NATURAL ℕ #-}
{-# BUILTIN ZERO zero #-}
{-# BUILTIN SUC succ #-}
\end{code}
}
В Agda функции можно определять как mixfix операторы.
Пример — сложение натуральных чисел:
\begin{code}
_+_ : ℕ → ℕ → ℕ
zero + b = b
succ a + b = succ (a + b)
\end{code}
Символы подчеркивания обозначают места для аргументов.
% Система типов \textit{Agda} позволяет ... 
% В отличие от \textit{Haskell}, в \textit{Agda} имеется ... 

Зависимые типы позволяют определять типы, зависящие (индексированные) от значений
других типов. Пример — список, индексированный своей длиной:
\begin{code}
data Vec (A : Set) : ℕ → Set where
  nil  : Vec A zero
  cons : ∀ {n} → A → Vec A n → Vec A (succ n)
\end{code}
В фигурные скобки заключаются неявные аргументы.

\subsection{Сопоставление с образцом по типам с индексами}

Такое определение \D{Vec} позволяет нам описать функцию $ \F{head} $ для такого списка, которая не может бросить исключение:
\begin{code}
head : ∀ {A} {n} → Vec A (succ n) → A
\end{code}
У аргумента функции $ \F{head} $ тип $ \D{Vec}\,A\,(\DC{succ}\,n) $, то есть вектор, в котором есть хотя бы один элемент.
Это позволяет произвести сопоставление с образцом только по конструктору $ \DC{cons} $:
\begin{code}
head (cons a as) = a
\end{code}

Перепишем тип данных \D{Vec} в немного другом виде — заменим индекс на параметр:
\AgdaHide{
\begin{code}
infix 4 _≡_
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x
\end{code}
}
\begin{code}
data Vec-ni (A : Set) (n : ℕ) : Set where
  nil  : (n ≡ zero) → Vec-ni A n
  cons : ∀ {k} → (n ≡ succ k) → A → Vec-ni A k → Vec-ni A n
\end{code}

% \begin{flalign*}
% \forall n .& \\
% \quad [] &\colon (n \equiv zero) \to Vec~A~n \\
% \quad \_::\_ &\colon \forall k . \to (n \equiv succ~k) \to A \to Vec~A~k \to Vec~A~n
% \end{flalign*}

Теперь конструкторы \DC{nil} и \DC{cons} явно требуют
доказательства о длине вектора \AgdaBound{n}.
Agda при сопоставлении с образцом на индексированных типах
генерирует эти доказательства с помощью унификации~\cite{McBrideView, Pfenning91Unification}.
В определении функции \F{head} тип аргумента унифицируется с типами конструкторов
типа данных \D{Vec} и, так как не существует \AgdaBound{k} такого,
что \DC{zero} $\equiv$ \DC{succ} \AgdaBound{k},
сопоставление производится только по конструктору \DC{cons}.
