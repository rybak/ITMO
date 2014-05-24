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

Такое определение позволяет нам описать функцию $ \F{head} $ для такого списка, которая не может бросить исключение:
\begin{code}
head : ∀ {A} {n} → Vec A (succ n) → A
\end{code}
У аргумента функции $ \F{head} $ тип $ \D{Vec}\,A\,(\DC{succ}\,n) $, то есть вектор, в котором есть хотя бы один элемент.
Это позволяет произвести сопоставление с образцом только по конструктору $ \DC{cons} $:
\begin{code}
head (cons a as) = a
\end{code}

