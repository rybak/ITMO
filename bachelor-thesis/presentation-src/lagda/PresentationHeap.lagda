\AgdaHide{
\begin{code}
module PresentationHeap where
open import AgdaDescription
\end{code}

}
\AgdaHide{
\begin{code}
module Level where
  postulate Level : Set
  postulate lzero : Level
  postulate lsucc : Level → Level
  postulate _⊔_   : Level → Level → Level
  infixl 6 _⊔_
  {-# BUILTIN LEVEL     Level #-}
  {-# BUILTIN LEVELZERO lzero #-}
  {-# BUILTIN LEVELSUC  lsucc #-}
  {-# BUILTIN LEVELMAX  _⊔_   #-}
open Level
_∘_ : ∀ {a b c}
    → {A : Set a} {B : Set b} {C : Set c}
    → (B → C) → (A → B) → (A → C)
f ∘ g = λ x → f (g x)
flip : ∀ {a b c}
     → {A : Set a} {B : Set b} {C : A → B → Set c} 
     → ((x : A) → (y : B) → C x y)
     → ((y : B) → (x : A) → C x y)
flip f x y = f y x
\end{code}
}

\AgdaHide{
\begin{frame}
\frametitle{}
Пустой тип
\begin{code}
data ⊥ : Set where -- нет конструкторов
\end{code}
Логическое отрицание
\begin{code}
¬ : ∀ {a} → Set a → Set a
¬ P = P → ⊥
\end{code}
Контрапозиция
\begin{code}
contraposition : ∀ {A B : Set}
  → (A → B) → (¬ B → ¬ A)
contraposition f ¬b a = ¬b (f a)
\end{code}
\end{frame}
}

\AgdaHide{
\begin{code}
cong : ∀ {A B : Set} → ∀ (f : A → B) {x y}
  → x ≡ y → f x ≡ f y -- Конгруэнтность
cong f refl = refl
\end{code}
\begin{frame}
\begin{code}
record Σ {a b} (A : Set a) (B : A → Set b)
  : Set (a ⊔ b) where -- Зависимая пара
  constructor _,_
  field fst : A ; snd : B fst
open Σ
_×_ : ∀ {a b} (A : Set a) → (B : Set b) → Set (a ⊔ b)
A × B = Σ A (λ _ → B) -- Декартово произведение
infixr 5 _×_ _,_
\end{code}
\end{frame}
}
\AgdaHide{
\begin{frame}
  \frametitle{OR}
\begin{code}
data OR (A B : Set) : Set where
  orA : A → OR A B
  orB : B → OR A B
\end{code}
\end{frame}
}

\begin{frame}
  \frametitle{Отношения}
\begin{code}
Rel₂ : Set → Set₁
Rel₂ A = A → A → Set
\end{code}
\end{frame}

\AgdaHide{
\begin{code} 
flip₁ : ∀ {A B : Set} {C : Set₁}
  → (A → B → C) → B → A → C
flip₁ f a b = f b a
\end{code} 
}
\begin{frame}
  \frametitle{Трихотомия}
\begin{code}
data Tri {A : Set} (_<_ _==_ _>_ : Rel₂ A) (a b : A)
  : Set where
  tri< :   (a < b) → ¬ (a == b) → ¬ (a > b)
    → Tri _<_ _==_ _>_ a b -- меньше
  tri= : ¬ (a < b) →   (a == b) → ¬ (a > b)
    → Tri _<_ _==_ _>_ a b -- равно
  tri> : ¬ (a < b) → ¬ (a == b) →   (a > b)
    → Tri _<_ _==_ _>_ a b -- больше
\end{code}
\pause
\begin{code}
Cmp : {A : Set} → Rel₂ A → Rel₂ A → Set
Cmp {A} _<_ _==_ = ∀ (x y : A) →
  Tri (_<_) (_==_) (flip₁ _<_) x y
\end{code} 
\end{frame}

\AgdaHide{
\begin{frame}
  \frametitle{Пример отношения}
\begin{code}
data _ℕ≤_ : Rel₂ ℕ where
  z≤n : ∀ {n} → zero ℕ≤ n
  s≤s : ∀ {n m} → n ℕ≤ m → succ n ℕ≤ succ m

_ℕ<_ _ℕ≥_ _ℕ>_ : Rel₂ ℕ
n ℕ< m = succ n ℕ≤ m
n ℕ> m = m ℕ< n
n ℕ≥ m = m ℕ≤ n
\end{code}
\end{frame}

\begin{frame}
  \frametitle{Пример компаратора}
\begin{code}
lemma-succ-≡ : ∀ {n} {m}
  → succ n ≡ succ m → n ≡ m
lemma-succ-≡ refl = refl
lemma-succ-≤ : ∀ {n} {m}
  → succ (succ n) ℕ≤ succ m → succ n ℕ≤ m
lemma-succ-≤ (s≤s r) = r
\end{code} 
\end{frame}

\begin{frame}
\begin{code}
cmpℕ : Cmp {ℕ} _ℕ<_ _≡_
cmpℕ zero (zero) = tri= (λ ()) refl (λ ())
cmpℕ zero (succ y) = tri< (s≤s z≤n) (λ ()) (λ ())
cmpℕ (succ x) zero = tri> (λ ()) (λ ()) (s≤s z≤n)
cmpℕ (succ x) (succ y) with cmpℕ x y
... | tri<  a ¬b ¬c = tri< (s≤s a)
  (contraposition lemma-succ-≡ ¬b)
  (contraposition lemma-succ-≤ ¬c)
... | tri> ¬a ¬b  c = tri>
  (contraposition lemma-succ-≤ ¬a)
  (contraposition lemma-succ-≡ ¬b) (s≤s c)
... | tri= ¬a  b ¬c = tri=
  (contraposition lemma-succ-≤ ¬a)
  (cong succ b) (contraposition lemma-succ-≤ ¬c)
\end{code} 
\end{frame}
}

\begin{frame}
  \frametitle {Свойства отношений}
Транзитивность 
\begin{code}
Trans : {A : Set} → Rel₂ A → Set
Trans {A} _rel_ = {a b c : A}
  → (a rel b) → (b rel c) → (a rel c)
\end{code}
Симметричность
\begin{code}
Symmetric : ∀ {A : Set} → Rel₂ A → Set
Symmetric _rel_ = ∀ {a b} → a rel b → b rel a
\end{code}
\end{frame}

\begin{frame}
\begin{code}
_Respects_ : ∀ {ℓ} {A : Set}
  → (A → Set ℓ) → Rel₂ A → Set _
P Respects _rel_ = ∀ {x y} → x rel y → P x → P y

_Respects₂_ : ∀ {A : Set}
  → Rel₂ A → Rel₂ A → Set
P Respects₂ _rel_ =
  (∀ {x} → P x      Respects _rel_) ×
  (∀ {y} → flip P y Respects _rel_)
\end{code}
\end{frame}

\section{<=}
\begin{frame}
  \frametitle{Обобщенное <=}
\begin{code}
data _<=_ {A : Set}
  {_<_ : Rel₂ A}
  {_==_ : Rel₂ A} : Rel₂ A where
  le : ∀ {x y} → x < y → x <= y
  eq : ∀ {x y} → x == y → x <= y
\end{code}
\end{frame}

\AgdaHide{
\begin{code}
min : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A}
  → (cmp : Cmp _<_ _==_) → A → A → A
min cmp x y with cmp x y
... | tri< _ _ _ = x
... | _ = y
\end{code}
}
\AgdaHide{
\begin{code}
min3 : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A}
  → (cmp : Cmp _<_ _==_) → A → A → A → A
min3 cmp x y z with cmp x y
... | tri< _ _ _ = min cmp x z
... | _ = min cmp y z
\end{code}
}
\AgdaHide{
\begin{code}
lemma-<=min : {A : Set}
  {_<_ : Rel₂ A}{_==_ : Rel₂ A}
  {cmp : Cmp _<_ _==_} {a b c : A}
  → (_<=_ {_<_ = _<_} {_==_} a b)
  → (_<=_ {_<_ = _<_} {_==_} a c)
  → (_<=_ {_<_ = _<_} {_==_} a (min cmp b c))
\end{code}
\begin{code}
lemma-<=min3 : {A : Set}
  {_<_ : Rel₂ A}{_==_ : Rel₂ A}
  {cmp : Cmp _<_ _==_} {x a b c : A}
  → (_<=_ {_<_ = _<_} {_==_} x a)
  → (_<=_ {_<_ = _<_} {_==_} x b)
  → (_<=_ {_<_ = _<_} {_==_} x c)
  → (_<=_ {_<_ = _<_} {_==_} x (min3 cmp a b c))
\end{code}
}
\AgdaHide{
\begin{code}
lemma-<=min {cmp = cmp} {_} {b} {c}
  ab ac with cmp b c
... | tri< _ _ _ = ab
... | tri= _ _ _ = ac
... | tri> _ _ _ = ac
\end{code}
}
\AgdaHide{
\begin{frame}
Доказательство \F{lemma-<=min3}
\begin{code}
lemma-<=min3 {cmp = cmp} {x} {a} {b} {c}
  xa xb xc with cmp a b
... | tri< _ _ _ = lemma-<=min {cmp = cmp} xa xc
... | tri= _ _ _ = lemma-<=min {cmp = cmp} xb xc
... | tri> _ _ _ = lemma-<=min {cmp = cmp} xb xc
\end{code}
\end{frame}
}

\begin{frame}
  \frametitle{Свойства <=}
\begin{code}
resp<= : {A : Set} {_<_ : Rel₂ A}
  {_==_ : Rel₂ A}
  → (resp : _<_ Respects₂ _==_)
  → (trans== : Trans _==_)
  → (sym== : Symmetric _==_)
  → (_<=_ {A}{_<_}{_==_}) Respects₂ _==_
\end{code}
% Транзитивность \D{\_<=\_}.
\begin{code}
trans<= : {A : Set}
  {_<_ : Rel₂ A} {_==_ : Rel₂ A}
  → _<_ Respects₂ _==_ → Symmetric _==_
  → Trans _==_ → Trans _<_
  → Trans (_<=_ {A}{_<_}{_==_})
\end{code}
\end{frame}

\AgdaHide{
\begin{code}
resp<= {A}{_<_}{_==_} resp trans sym = left , right where
  left : ∀ {a b c : A} → b == c → a <= b → a <= c
  left b=c (le a<b) = le (fst resp b=c a<b)
  left b=c (eq a=b) = eq (trans a=b b=c)
  right : ∀ {a b c : A} → b == c → b <= a → c <= a
  right b=c (le a<b) = le (snd resp b=c a<b)
  right b=c (eq a=b) = eq (trans (sym b=c) a=b)
\end{code}
\begin{code}
trans<= r s t== t< (le a<b) (le b<c)
  = le (t< a<b b<c)
trans<= r s t== t< (le a<b) (eq b=c)
  = le (fst r b=c a<b)
trans<= r s t== t< (eq a=b) (le b<c)
  = le (snd r (s a=b) b<c)
trans<= r s t== t< (eq a=b) (eq b=c)
  = eq (t== a=b b=c)
\end{code}
}

\section{Куча}

\begin{frame}
  \frametitle{Заголовок модуля}
Требования к исходному типу
\begin{code}
module Heap (A : Set) (_<_ _==_ : Rel₂ A)
  (cmp : Cmp _<_ _==_)
  (sym== : Symmetric _==_)
  (trans== : Trans _==_)
  (trans< : Trans _<_)
  (resp : _<_ Respects₂ _==_)
  where
\end{code}
\end{frame}

\begin{frame}
  \frametitle{Расширение}
\begin{code}
  data expanded (A : Set) : Set where
    # : A → expanded A -- элемент исходного типа
    top : expanded A -- элемент расширение
\end{code}
\pause
Расширенные отношения
\begin{code}
  data _<E_ : Rel₂ (expanded A) where
    base : ∀ {x y : A} → x < y → (# x) <E (# y)
    ext  : ∀ {x : A} → (# x) <E top

  data _=E_ : Rel₂ (expanded A) where
    base : ∀ {x y} → x == y → (# x) =E (# y)
    ext  : top =E top
\end{code}
\end{frame}

\begin{frame}
  \frametitle{Свойства расширенных отношений}
\begin{code}
  lemma-<E : ∀ {x} {y} → (# x) <E (# y) → x < y
  trans<E : Trans _<E_

  lemma-=E : ∀ {x} {y} → (# x) =E (# y) → x == y
  sym=E   : Symmetric _=E_
  trans=E : Trans _=E_

  respE : _<E_ Respects₂ _=E_

  cmpE : Cmp {expanded A} _<E_ _=E_
\end{code}
\end{frame}
\AgdaHide{
\begin{code}
  lemma-<E (base r) = r
  trans<E {# _} {# _} {# _} a<b b<c =
    base (trans< (lemma-<E a<b) (lemma-<E b<c))
  trans<E {# _} {# _} {top} _  _  = ext
  trans<E {# _} {top} {_}   _  ()
  trans<E {top} {_}   {_}   () _

  lemma-=E (base r) = r
  sym=E (base a=b) = base (sym== a=b)
  sym=E ext = ext
  trans=E (base a=b) (base b=c) = base (trans== a=b b=c)
  trans=E ext ext = ext

  respE = left , right where
    left : ∀ {a b c : expanded A} → b =E c → a <E b → a <E c
    left {# _} {# _} {# _} (base r1) (base r2) = base (fst resp r1 r2)
    left {# _} {top} {top} ext ext = ext
    left {_} {# _} {top} () _
    left {_} {top} {# _} () _
    left {top} {_} {_}   _ ()
    right : ∀ {a b c : expanded A} → b =E c → b <E a → c <E a
    right {# _} {# _} {# _} (base r1) (base r2) = base (snd resp r1 r2)
    right {top} {# _} {# _} _ ext = ext
    right {_} {# _} {top} () _
    right {_} {top} {_} _ ()
\end{code}
}
\AgdaHide{
\begin{code}
  cmpE (# x) (# y) with cmp x y
  cmpE (# x) (# y) | tri< a b c = tri<
    (base a)
    (contraposition lemma-=E b)
    (contraposition lemma-<E c)
  cmpE (# x) (# y) | tri= a b c = tri=
    (contraposition lemma-<E a)
    (base b)
    (contraposition lemma-<E c)
  cmpE (# x) (# y) | tri> a b c = tri>
    (contraposition lemma-<E a)
    (contraposition lemma-=E b)
    (base c)
\end{code}
\begin{code}
  cmpE (# x) top = tri< ext (λ ()) (λ ())
  cmpE top (# y) = tri> (λ ()) (λ ()) ext
  cmpE top top   = tri= (λ ()) ext (λ ())
\end{code}
}

\begin{frame}
  \frametitle{$ \_\leq\_ $}
\begin{code}
  _≤_ : Rel₂ (expanded A)
  _≤_ = _<=_ {expanded A} {_<E_} {_=E_}

  trans≤ : Trans _≤_
  trans≤ = trans<= respE sym=E trans=E trans<E

  resp≤ : _≤_ Respects₂ _=E_
  resp≤ = resp<= respE trans=E sym=E
\end{code}
\end{frame}

\begin{frame}
  \frametitle{minE}
\begin{code}
  minE : (x y : expanded A) → expanded A
  minE = min cmpE
  min3E : (expanded A) → (expanded A)
    → (expanded A) → (expanded A)
  min3E x y z = min3 cmpE x y z
\end{code}
\begin{code}
  lemma-<=minE : ∀ {a b c} → 
    a ≤ b → a ≤ c → a ≤ (minE b c)

  lemma-<=min3E : ∀ {x a b c}
    → x ≤ a → x ≤ b → x ≤ c → x ≤ (min3E a b c)
\end{code}
\AgdaHide{
\begin{code}
  lemma-<=minE = 
    lemma-<=min {expanded A}{_<E_}{_=E_}{cmpE}
  lemma-<=min3E =
    lemma-<=min3 {expanded A}{_<E_}{_=E_}{cmpE}
\end{code}
}
\end{frame} % minE

\begin{frame}
  \frametitle{Heap}
\begin{code}
  data HeapState : Set where
    full almost : HeapState

  data Heap : (expanded A) -- минимум
    → (h : ℕ) -- высота
    → HeapState -- заполненность
    → Set where
    eh : Heap top zero full -- Пустая куча
\end{code}
\end{frame} % Heap

\begin{frame}
\begin{code}
    nf : ∀ {n} {x y} → (p : A)
      → (i : (# p) ≤ x) → (j : (# p) ≤ y)
      → (a : Heap x n full)
      → (b : Heap y n full) -- a b одной высоты
      → Heap (# p) (succ n) full
\end{code}
\begin{center}
\includegraphics{pic/p-nodes-1.pdf}
\end{center}
\end{frame} % nf

\begin{frame}
\begin{code}
    nd : ∀ {n} {x y} → (p : A)
      → (i : (# p) ≤ x) → (j : (# p) ≤ y)
      → (a : Heap x (succ n) full)
      → (b : Heap y n full) -- a b разной высоты
      → Heap (# p) (succ (succ n)) almost
\end{code}
\begin{center}
\includegraphics{pic/p-nodes-2.pdf}
\end{center}
\end{frame} % nd

\begin{frame}
\begin{code}
    nl : ∀ {n} {x y} → (p : A)
      → (i : (# p) ≤ x) → (j : (# p) ≤ y)
      → (a : Heap x (succ n) almost)
      → (b : Heap y n full) -- b - полная
      → Heap (# p) (succ (succ n)) almost
\end{code}
\begin{center}
\includegraphics{pic/p-nodes-3.pdf}
\end{center}
\end{frame} % nl

\begin{frame}
\begin{code}
    nr : ∀ {n} {x y} → (p : A)
      → (i : (# p) ≤ x) → (j : (# p) ≤ y)
      → (a : Heap x (succ n) full) -- a - полная
      → (b : Heap y (succ n) almost)
      → Heap (# p) (succ (succ n)) almost
\end{code}
\begin{center}
\includegraphics{pic/p-nodes-4.pdf}
\end{center}
\end{frame} % nr

\AgdaHide{
\begin{frame}
  \frametitle{}
Высота любой неполной кучи больше нуля.
\begin{code}
  lemma-almost-height : ∀ {m h}
    → Heap m h almost → h ℕ> 0
  lemma-almost-height (nd _ _ _ _ _) = s≤s z≤n
  lemma-almost-height (nl _ _ _ _ _) = s≤s z≤n
  lemma-almost-height (nr _ _ _ _ _) = s≤s z≤n
\end{code}
\end{frame} % lah
}
\AgdaHide{
\begin{frame}
  \frametitle{peekMin}
\begin{code}
  peekMin : ∀ {m h s} → Heap m h s → (expanded A)
  peekMin eh = top
  peekMin (nd p _ _ _ _) = # p
  peekMin (nf p _ _ _ _) = # p
  peekMin (nl p _ _ _ _) = # p
  peekMin (nr p _ _ _ _) = # p
\end{code}
\end{frame} peek
}

\AgdaHide{
\begin{frame}
  \frametitle{Вспомогательные леммы}
\begin{code}
  lemma-resp : ∀ {x y a b}
    → x == y → (# x) ≤ a → (# x) ≤ b
    → (# y) ≤ minE a b
  lemma-resp x=y i j = lemma-<=minE
    (snd resp≤ (base x=y) i)
    (snd resp≤ (base x=y) j)
  lemma-trans : ∀ {x y a b}
    → y < x → (# x) ≤ a → (# x) ≤ b
    → (# y) ≤ minE a b
  lemma-trans y<x i j = lemma-<=minE
    (trans≤ (le (base y<x)) i)
    (trans≤ (le (base y<x)) j)
\end{code}
\end{frame} % lemmas
}

\begin{frame}
  \frametitle{Вставка}
Вставка в полную кучу
\begin{code}
  finsert : ∀ {h m} → (z : A)
    → Heap m h full
    → Σ HeapState (Heap (minE m (# z)) (succ h))
\end{code}
Вставка в неполную кучу
\begin{code}
  ainsert : ∀ {h m} → (z : A)
    → Heap m h almost
    → Σ HeapState (Heap (minE m (# z)) h)
\end{code}
\end{frame} % inserts

\AgdaHide{
\begin{code}
  finsert = {!!}
\end{code}
}
\AgdaHide{
\begin{code}
  ainsert = {!!}
\end{code}
} 

\section{Слияние куч} 
\begin{frame}
  \frametitle{fmerge}
Слияние двух полных куч одной высоты
\begin{code}
  fmerge : ∀ {x y h}
    → Heap x h full → Heap y h full
    → OR (Heap x zero full × (x ≡ y) × (h ≡ zero))
         (Heap (minE x y) (succ h) almost)
\end{code}
\end{frame} % fmerge

\begin{frame}
  \frametitle{makeH}
Составление полной кучи высотой $h+1$ из двух куч высотой $h$ и одного элемента
\begin{code}
  makeH : ∀ {x y h} → (p : A)
    → Heap x h full → Heap y h full
    → Heap (min3E x y (# p)) (succ h) full
\end{code}
\end{frame} % makeH

\begin{frame}
  \frametitle{ndmerge}
Слияние поддеревьев \DC{nd}
\begin{code}
  ndmerge : ∀ {x y h}
    → Heap x (succ (succ h)) full
    → Heap y (succ h) full
    → Heap (minE x y) (succ (succ (succ h))) almost
\end{code}
\end{frame} % ndmerge

\begin{frame}
  \frametitle{afmerge}
Слияние неполной кучи высотой $h+2$ и полной кучи высотой $h+1$ или $h+2$
\begin{code}
  afmerge : ∀ {h x y}
    → Heap x (succ (succ h)) almost
    → OR (Heap y (succ h) full)
         (Heap y (succ (succ h)) full)
    → OR (Heap (minE x y) (succ (succ h)) full)
         (Heap (minE x y) (succ (succ (succ h))) almost)
\end{code}
\end{frame} % afmerge

\AgdaHide{
\begin{code}
  fmerge = {!!}
\end{code}
}
\AgdaHide{
\begin{code}
  makeH = {!!}
\end{code}
}
\AgdaHide{
\begin{code}
  ndmerge = {!!}
\end{code}
}
\AgdaHide{
\begin{code}
  afmerge = {!!}
\end{code}
}

\begin{frame}
  \frametitle{Извлечение минимума}
Извлечение минимума из полной кучи
\begin{code}
  fpop : ∀ {m h} → Heap m (succ h) full
    → OR
    (Σ (expanded A) 
       (λ x → (Heap x (succ h) almost) × (m ≤ x)))
    (Heap top h full)
\end{code}
Извлечение минимума из неполной кучи
\begin{code}
  apop : ∀ {m h} → Heap m (succ h) almost
    → OR (Σ (expanded A)
        (λ x → (Heap x (succ h) almost) × (m ≤ x)))
         (Σ (expanded A)
        (λ x → (Heap x h full) × (m ≤ x)))
\end{code}
\end{frame} % pops
\AgdaHide{
\begin{code}
  apop = {!!}
\end{code}
}
\AgdaHide{
\begin{code}
  fpop = {!!}
\end{code}
}
 
