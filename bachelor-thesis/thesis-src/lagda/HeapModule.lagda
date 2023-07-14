\section{Вспомогательные определения}

\subsection{Общие определения}
Некоторые общеизвестные определения заимствованы из стандартной библиотеки
Agda~\cite{AgdaSLib}.

\begin{code}
module HeapModule where
\end{code} Тип данных для пустого типа.
У этого типа нет конструкторов, и, как следствие, нет термов,
населяющих этот тип.
\begin{code}
data ⊥ : Set where

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

module Function where
\end{code}
Композиция функций.
\begin{code}
  _∘_ : ∀ {a b c} → {A : Set a} {B : Set b} {C : Set c}
      → (B → C) → (A → B) → (A → C)
  f ∘ g = λ x → f (g x)
  flip : ∀ {a b c} → {A : Set a} {B : Set b} {C : A → B → Set c} 
       → ((x : A) → (y : B) → C x y) → ((y : B) → (x : A) → C x y)
  flip f x y = f y x
open Function public
module Logic where
\end{code}
\begin{minipage}{\textwidth}
Из элемента пустого типа следует что-угодно.
\begin{code}
  ⊥-elim : ∀ {a} {Whatever : Set a} → ⊥ → Whatever
  ⊥-elim ()
\end{code} Логическое отрицание.
\begin{code}
  ¬ : ∀ {a} → Set a → Set a
  ¬ P = P → ⊥
\end{code}
\end{minipage}

\begin{minipage}{\textwidth}
\begin{code}
  private
   module DummyAB {a b} {A : Set a} {B : Set b} where
\end{code} Контрадикция, противоречие: из $A$ и $\neg A$ можно получить любое $B$.
\begin{code}
    contradiction : A → ¬ A → B
    contradiction a ¬a = ⊥-elim (¬a a)
\end{code}
\end{minipage}
Контрапозиция
\begin{code}
    contraposition : (A → B) → (¬ B → ¬ A)
    contraposition = flip _∘_
  open DummyAB public
open Logic public
\end{code}
Определения интуиционистской теории типов Матрина-Лёфа~\cite{MLTT}.
\begin{code}
module MLTT where
\end{code}
Пропозициональное равенство из интуиционистской теории типов.
\begin{code}
  infix 4 _≡_
  data _≡_ {a} {A : Set a} (x : A) : A → Set a where
    refl : x ≡ x
  {-# BUILTIN EQUALITY _≡_ #-}
  {-# BUILTIN REFL    refl #-}
\end{code} Тип-сумма — зависимая пара.
\begin{code}
  record Σ {a b} (A : Set a) (B : A → Set b) : Set (a ⊔ b) where
    constructor _,_
    field fst : A ; snd : B fst
  open Σ public
\end{code} Декартово произведение — частный случай зависимой пары,
Второй индекс игнорирует передаваемое ему значение.
\begin{code}
  _×_ : ∀ {a b} (A : Set a) → (B : Set b) → Set (a ⊔ b)
  A × B = Σ A (λ _ → B)
  infixr 5 _×_ _,_
\end{code}
\begin{minipage}{\textwidth}
\begin{code}
  module ≡-Prop where
   private
    module DummyA {a b} {A : Set a} {B : Set b} where
\end{code} Конгруэнтность пропозиционального равенства.
\begin{code}
      cong : ∀ (f : A → B) {x y} → x ≡ y → f x ≡ f y
      cong f refl = refl
    open DummyA public
  open ≡-Prop public
open MLTT public
\end{code}
\end{minipage}

\subsection{Определение отношений и доказательства их свойств}
Чтобы задать порядок элементов в куче, нужно уметь сравнивать элементы.
Зададим отношения на этих элементах.
\begin{code}
Rel₂ : Set → Set₁
Rel₂ A = A → A → Set
\end{code}
\begin{minipage}{\textwidth}
Трихотомичность отношений меньше, равно и больше:
одновременно два элемента могут принадлежать только одному отношению из трех.
\begin{code}
data Tri {A : Set} (_<_ _==_ _>_ : Rel₂ A) (a b : A) : Set where
  tri< :   (a < b) → ¬ (a == b) → ¬ (a > b) → Tri _<_ _==_ _>_ a b
  tri= : ¬ (a < b) →   (a == b) → ¬ (a > b) → Tri _<_ _==_ _>_ a b
  tri> : ¬ (a < b) → ¬ (a == b) →   (a > b) → Tri _<_ _==_ _>_ a b
\end{code}
\end{minipage}
\begin{minipage}{\textwidth}
Введем упрощенный предикат, использующий только два отношения
— меньше и равенство. 
Отношение больше заменяется отношением меньше с переставленными аргументами.
\begin{code}
flip₁ : ∀ {A B : Set} {C : Set₁} → (A → B → C) → B → A → C
flip₁ f a b = f b a

Cmp : {A : Set} → Rel₂ A → Rel₂ A → Set
Cmp {A} _<_ _==_ = ∀ (x y : A) → Tri (_<_) (_==_) (flip₁ _<_) x y
\end{code}
\end{minipage}
\begin{minipage}{\textwidth}
Задавать высоту кучи будем натуральными числами.
\begin{code}
data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ
{-# BUILTIN NATURAL ℕ #-}
{-# BUILTIN ZERO zero #-}
{-# BUILTIN SUC succ #-}
\end{code}
\end{minipage}
Тип данных для отношения меньше или равно на натуральных числах.
\begin{code}
data _ℕ≤_ : Rel₂ ℕ where
  z≤n : ∀ {n} → zero ℕ≤ n
  s≤s : ∀ {n m} → n ℕ≤ m → succ n ℕ≤ succ m
\end{code} Все остальные отношения определяются через \F{ \_ℕ≤\_ }.
\begin{code}
_ℕ<_ _ℕ≥_ _ℕ>_ : Rel₂ ℕ
n ℕ< m = succ n ℕ≤ m
n ℕ> m = m ℕ< n
n ℕ≥ m = m ℕ≤ n
\end{code} В качестве примера компаратора —
доказательство трихотомичности для отношения меньше для натуральных чисел.
\begin{code}
lemma-succ-≡ : ∀ {n} {m} → succ n ≡ succ m → n ≡ m
lemma-succ-≡ refl = refl
lemma-succ-≤ : ∀ {n} {m} → succ (succ n) ℕ≤ succ m → succ n ℕ≤ m
lemma-succ-≤ (s≤s r) = r

cmpℕ : Cmp {ℕ} _ℕ<_ _≡_
cmpℕ zero (zero) = tri= (λ ()) refl (λ ())
cmpℕ zero (succ y) = tri< (s≤s z≤n) (λ ()) (λ ())
cmpℕ (succ x) zero = tri> (λ ()) (λ ()) (s≤s z≤n)
cmpℕ (succ x) (succ y) with cmpℕ x y
... | tri<  a ¬b ¬c = tri< (s≤s a) (contraposition lemma-succ-≡ ¬b)
  (contraposition lemma-succ-≤ ¬c)
... | tri> ¬a ¬b  c = tri> (contraposition lemma-succ-≤ ¬a)
  (contraposition lemma-succ-≡ ¬b) (s≤s c)
... | tri= ¬a  b ¬c = tri= (contraposition lemma-succ-≤ ¬a)
  (cong succ b) (contraposition lemma-succ-≤ ¬c)
\end{code}
\begin{minipage}{\textwidth}
Определим типы данных для задания свойств отношений.
Транзитивность отношения.
\begin{code}
Trans : {A : Set} → Rel₂ A → Set
Trans {A} _rel_ = {a b c : A} → (a rel b) → (b rel c) → (a rel c)
\end{code}
\end{minipage}
Симметричность отношения.
\begin{code}
Symmetric : ∀ {A : Set} → Rel₂ A → Set
Symmetric _rel_ = ∀ {a b} → a rel b → b rel a
\end{code} Предикат $P$ учитывает (соблюдает) отношение \AgdaOperator{ \_rel\_ }.
\begin{code}
_Respects_ : ∀ {ℓ} {A : Set} → (A → Set ℓ) → Rel₂ A → Set _
P Respects _rel_ = ∀ {x y} → x rel y → P x → P y
\end{code} Отношение $P$ соблюдает отношение \AgdaOperator{ \_rel\_ }.
\begin{code}
_Respects₂_ : ∀ {A : Set} → Rel₂ A → Rel₂ A → Set
P Respects₂ _rel_ =
  (∀ {x} → P x      Respects _rel_) ×
  (∀ {y} → flip P y Respects _rel_)
\end{code} Тип данных для обобщенного отношения меньше или равно.
\begin{code}
data _<=_ {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A} : Rel₂ A where
  le : ∀ {x y} → x < y → x <= y
  eq : ∀ {x y} → x == y → x <= y
\end{code} Обобщенные функции минимум и максимум.
\begin{code}
min max : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A}
  → (cmp : Cmp _<_ _==_) → A → A → A
min cmp x y with cmp x y
... | tri< _ _ _ = x
... | _ = y
max cmp x y with cmp x y
... | tri> _ _ _ = x
... | _ = y
\end{code} Лемма: элемент меньше или равный двух других элементов
меньше или равен минимума из них.
\begin{code}
lemma-<=min : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A}
  {cmp : Cmp _<_ _==_} {a b c : A}
  → (_<=_ {_<_ = _<_} {_==_} a b)
  → (_<=_ {_<_ = _<_} {_==_} a c)
  → (_<=_ {_<_ = _<_} {_==_} a (min cmp b c))
lemma-<=min {cmp = cmp} {_} {b} {c} ab ac with cmp b c
... | tri< _ _ _ = ab
... | tri= _ _ _ = ac
... | tri> _ _ _ = ac
\end{code} Функция — минимум из трех элементов.
\begin{code}
min3 : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A}
  → (cmp : Cmp _<_ _==_) → A → A → A → A
min3 cmp x y z with cmp x y
... | tri< _ _ _ = min cmp x z
... | _ = min cmp y z
\end{code} Аналогичная предыдущей лемма для минимума из трех элементов.
\begin{code}
lemma-<=min3 : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A}
  {cmp : Cmp _<_ _==_} {x a b c : A}
  → (_<=_ {_<_ = _<_} {_==_} x a)
  → (_<=_ {_<_ = _<_} {_==_} x b)
  → (_<=_ {_<_ = _<_} {_==_} x c)
  → (_<=_ {_<_ = _<_} {_==_} x (min3 cmp a b c))
lemma-<=min3 {cmp = cmp} {x} {a} {b} {c} xa xb xc with cmp a b
... | tri< _ _ _ = lemma-<=min {cmp = cmp} xa xc
... | tri= _ _ _ = lemma-<=min {cmp = cmp} xb xc
... | tri> _ _ _ = lemma-<=min {cmp = cmp} xb xc
\end{code} Леммы \F{lemma-<=min} и \F{lemma-<=min3} понадобятся
при доказательстве соотношений между элементами,
из которых составляются новые кучи при их обработке.

Отношение \AgdaOperator{\_<=\_} соблюдает отношение равенства \AgdaOperator{\_==\_},
с помощью которого оно определено.
\begin{code}
resp<= : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A}
  → (resp : _<_ Respects₂ _==_) → (trans== : Trans _==_)
  → (sym== : Symmetric _==_)
  → (_<=_ {A}{_<_}{_==_}) Respects₂ _==_
resp<= {A}{_<_}{_==_} resp trans sym = left , right where
  left : ∀ {a b c : A} → b == c → a <= b → a <= c
  left b=c (le a<b) = le (fst resp b=c a<b)
  left b=c (eq a=b) = eq (trans a=b b=c)
  right : ∀ {a b c : A} → b == c → b <= a → c <= a
  right b=c (le a<b) = le (snd resp b=c a<b)
  right b=c (eq a=b) = eq (trans (sym b=c) a=b)
\end{code} Транзитивность отношения \AgdaOperator{\_<=\_}.
\begin{code}
trans<= : {A : Set} {_<_ : Rel₂ A} {_==_ : Rel₂ A}
  → _<_ Respects₂ _==_
  → Symmetric _==_ → Trans _==_ → Trans _<_
  → Trans (_<=_ {A}{_<_}{_==_})
trans<= r s t== t< (le a<b) (le b<c) = le (t< a<b b<c)
trans<= r s t== t< (le a<b) (eq b=c) = le (fst r b=c a<b)
trans<= r s t== t< (eq a=b) (le b<c) = le (snd r (s a=b) b<c)
trans<= r s t== t< (eq a=b) (eq b=c) = eq (t== a=b b=c)
\end{code}

\section{Модуль Heap}
Модуль, в котором мы определим структуру данных куча, параметризован
исходным типом, двумя отношениями, определенными для этого типа, \AgdaOperator{\_<\_} и \AgdaOperator{\_==\_}.
Также требуется симметричность и транзитивность \AgdaOperator{\_==\_},
транзитивность \AgdaOperator{\_<\_}, соблюдение отношением \AgdaOperator{\_<\_} отношения \AgdaOperator{\_==\_} и

\begin{code}
module Heap (A : Set) (_<_ _==_ : Rel₂ A) (cmp : Cmp _<_ _==_)
  (sym== : Symmetric _==_) (trans== : Trans _==_)
  (trans< : Trans _<_) (resp : _<_ Respects₂ _==_)
  where
\end{code}

\subsection{Расширение исходного типа}
Будем индексировать кучу минимальным элементом в ней,
для того, чтобы можно было строить инварианты порядка на куче исходя из
этих индексов.
Так как в пустой куче нет элементов, то мы не можем выбрать элемент,
который нужно указать в индексе. Чтобы решить эту проблему, расширим
исходный тип данных, добавив элемент, больший всех остальных.
Тип данных для расширения исходного типа. 
\begin{code}
  data expanded (A : Set) : Set where
    # : A → expanded A -- элемент исходного типа
    top : expanded A -- элемент расширение
\end{code} Теперь нам нужно аналогичным образом расширить отношения
заданные на множестве исходного типа.
Тип данных для расширения отношения меньше.
\begin{code}
  data _<E_ : Rel₂ (expanded A) where
    base : ∀ {x y : A} → x < y → (# x) <E (# y)
    ext  : ∀ {x : A} → (# x) <E top
\end{code} Вспомогательная лемма, извлекающая доказательство
для отношения элементов исходного типа из отношения для элементов
расширенного типа.
\begin{code}
  lemma-<E : ∀ {x} {y} → (# x) <E (# y) → x < y
  lemma-<E (base r) = r
\end{code} Расширенное отношение меньше — транзитивно.
\begin{code}
  trans<E : Trans _<E_
  trans<E {# _} {# _} {# _} a<b b<c =
    base (trans< (lemma-<E a<b) (lemma-<E b<c))
  trans<E {# _} {# _} {top} _  _  = ext
  trans<E {# _} {top} {_}   _  ()
  trans<E {top} {_}   {_}   () _
\end{code} Тип данных расширенного отношения равенства.
\begin{code}
  data _=E_ : Rel₂ (expanded A) where
    base : ∀ {x y} → x == y → (# x) =E (# y)
    ext  : top =E top
\end{code} Расширенное отношение равенства — симметрично и транзитивно.
\begin{code}
  sym=E   : Symmetric _=E_
  sym=E (base a=b) = base (sym== a=b)
  sym=E ext = ext
  trans=E : Trans _=E_
  trans=E (base a=b) (base b=c) = base (trans== a=b b=c)
  trans=E ext ext = ext
\end{code} Отношение \F{\_<E\_} соблюдает отношение \F{\_=E\_}.
\begin{code}
  respE : _<E_ Respects₂ _=E_
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
\begin{minipage}{\textwidth}
Отношение меньше-равно для расширенного типа.
\begin{code}
  _≤_ : Rel₂ (expanded A)
  _≤_ = _<=_ {expanded A} {_<E_} {_=E_}
\end{code}
\end{minipage}
Транзитивность меньше-равно следует из свойств
отношений \F{\_=E\_} и \F{\_<E\_}:
\begin{code}
  trans≤ : Trans _≤_
  trans≤ = trans<= respE sym=E trans=E trans<E
  resp≤ : _≤_ Respects₂ _=E_
  resp≤ = resp<= respE trans=E sym=E
\end{code} Вспомогательная лемма, извлекающая доказательство
равенства элементов исходного типа из равенства элементов
расширенного типа.
\begin{code}
  lemma-=E : ∀ {x} {y} → (# x) =E (# y) → x == y
  lemma-=E (base r) = r
\end{code} Трихотомичность для \F{\_<E\_} и \F{\_=E\_}.
\begin{code}
  cmpE : Cmp {expanded A} _<E_ _=E_
  cmpE (# x) (# y) with cmp x y
  cmpE (# x) (# y) | tri< a b c = 
      tri< (base a) (contraposition lemma-=E b) (contraposition lemma-<E c)
  cmpE (# x) (# y) | tri= a b c =
      tri= (contraposition lemma-<E a) (base b) (contraposition lemma-<E c)
  cmpE (# x) (# y) | tri> a b c =
      tri> (contraposition lemma-<E a) (contraposition lemma-=E b) (base c)
  cmpE (# x) top = tri< ext (λ ()) (λ ())
  cmpE top (# y) = tri> (λ ()) (λ ()) ext
  cmpE top top   = tri= (λ ()) ext (λ ())
\end{code} Функция — минимум для расширенного типа.
\begin{code}
  minE : (x y : expanded A) → expanded A
  minE = min cmpE
\end{code} Функция — минимум из трех элементов расширенного типа —
частный случай ранее определенной общей функции.
\begin{code}
  min3E : (expanded A) → (expanded A) → (expanded A) → (expanded A)
  min3E x y z = min3 cmpE x y z
\end{code} Леммы для сравнения с минимумами для элементов расширенного типа.
\begin{code}
  lemma-<=minE : ∀ {a b c} → a ≤ b → a ≤ c → a ≤ (minE b c)
  lemma-<=minE = lemma-<=min {expanded A}{_<E_}{_=E_}{cmpE}
  lemma-<=min3E : ∀ {x a b c} → x ≤ a → x ≤ b → x ≤ c
    → x ≤ (min3E a b c)
  lemma-<=min3E = lemma-<=min3 {expanded A}{_<E_}{_=E_}{cmpE}
\end{code} 

\subsection{Тип данных Heap}
Вспомогательный тип данных для индексации кучи — куча полная или почти заполненная.
\begin{code}
  data HeapState : Set where
    full almost : HeapState
\end{code} Тип данных для кучи, индексированный
минимальным элементом кучи, высотой и заполненностью.
\begin{code}
  data Heap : (expanded A) → (h : ℕ) → HeapState → Set where
\end{code} У пустой кучи минимальный элемент — \DC{top}, высота — ноль.
Пустая куча — полная.
\begin{code}
    eh : Heap top zero full
\end{code} Мы хотим в непустых кучах задавать порядок на элементах —
элемент в узле меньше либо равен элементов в поддеревьях.
Мы можем упростить этот инвариант, сравнивая элемент в узле
только с корнями поддеревьев.
Порядок кучи задается с помощью двух
элементов отношения \AgdaOperator{\_≤\_}: $i$ и $j$, которые говорят от том, что
значение в корне меньше-равно
значений в корнях левого и правого поддеревьев соответственно.
На рисунке~\ref{pic:heap-nodes} схематично изображены конструкторы
типа данных \D{Heap}.

Полная куча высотой $n + 1$ состоит из корня и
двух куч высотой $n$.
\begin{code}
    nf : ∀ {n} {x y} → (p : A) → (i : (# p) ≤ x) → (j : (# p) ≤ y)
        → (a : Heap x n full)
        → (b : Heap y n full)
        → Heap (# p) (succ n) full
\end{code} Куча высотой $n+2$, у которой нижний ряд заполнен до середины,
состоит из корня и двух полных куч: левая высотой $n+1$ и правая высотой $n$.
\begin{code}
    nd : ∀ {n} {x y} → (p : A) → (i : (# p) ≤ x) → (j : (# p) ≤ y)
        → (a : Heap x (succ n) full)
        → (b : Heap y n full)
        → Heap (# p) (succ (succ n)) almost
\end{code} Куча высотой $n+2$, у которой нижний ряд заполнен меньше, чем до середины,
состоит из корня и двух куч: левая неполная высотой $n+1$ и правая полная высотой $n$.
\begin{code}
    nl : ∀ {n} {x y} → (p : A) → (i : (# p) ≤ x) → (j : (# p) ≤ y)
        → (a : Heap x (succ n) almost)
        → (b : Heap y n full)
        → Heap (# p) (succ (succ n)) almost
\end{code} Неполная куча высотой $n+2$, у которой нижний ряд заполнен больше, чем до середины,
состоит из корня и двух куч: левая полная высотой $n+1$ и правая неполная высотой $n+1$.
\begin{code}
    nr : ∀ {n} {x y} → (p : A) → (i : (# p) ≤ x) → (j : (# p) ≤ y)
        → (a : Heap x (succ n) full)
        → (b : Heap y (succ n) almost)
        → Heap (# p) (succ (succ n)) almost
\end{code}
\begin{figure}
  \begin{center}
    \includegraphics[width=0.6\textwidth]{pic/nodes-1.pdf}
    \caption{Конструкторы типа данных \D{Heap}}
    \label{pic:heap-nodes}
  \end{center}
\end{figure}
\begin{minipage}{\textwidth}
\emph{Замечание}: высота любой неполной кучи больше нуля.
\begin{code}
  lemma-almost-height : ∀ {m h} → Heap m h almost → h ℕ> 0
  lemma-almost-height (nd _ _ _ _ _) = s≤s z≤n
  lemma-almost-height (nl _ _ _ _ _) = s≤s z≤n
  lemma-almost-height (nr _ _ _ _ _) = s≤s z≤n
\end{code}
\end{minipage}
\begin{minipage}{\textwidth}
Функция — просмотр минимума в куче.
\begin{code}
  peekMin : ∀ {m h s} → Heap m h s → (expanded A)
  peekMin eh = top
  peekMin (nd p _ _ _ _) = # p
  peekMin (nf p _ _ _ _) = # p
  peekMin (nl p _ _ _ _) = # p
  peekMin (nr p _ _ _ _) = # p
\end{code}
\end{minipage}

\subsection{Функции вставки в кучу}
Функция вставки элемента в полную кучу.
\begin{code}
  finsert : ∀ {h m} → (z : A) → Heap m h full
    → Σ HeapState (Heap (minE m (# z)) (succ h))
  finsert {0} z eh = full , nf z (le ext) (le ext) eh eh
  finsert {1} z (nf p i j eh eh) with cmp p z
  ... | tri< p<z _ _ = almost ,
    nd p (le (base p<z)) j (nf z (le ext) (le ext) eh eh) eh
  ... | tri= _ p=z _ = almost ,
    nd z (eq (base (sym== p=z))) (le ext) (nf p i j eh eh) eh
  ... | tri> _ _ z<p = almost ,
    nd z (le (base z<p)) (le ext) (nf p i j eh eh) eh
  finsert z (nf p i j (nf x i₁ j₁ a b) c) with cmp p z
  finsert z (nf p i j (nf x i₁ j₁ a b) c) | tri< p<z _ _
    with finsert z (nf x i₁ j₁ a b)
    | lemma-<=minE {# p} {# x} {# z} i (le (base p<z))
  ... | full   , newleft | l1 = almost , nd p l1 j newleft c
  ... | almost , newleft | l1 = almost , nl p l1 j newleft c
  finsert z (nf p i j (nf x i₁ j₁ a b) c) | tri= _ p=z _
    with finsert p (nf x i₁ j₁ a b)
    | lemma-<=minE (snd resp≤ (base p=z) i) (eq (base (sym== p=z)))
    | snd resp≤ (base p=z) j
  ... | full   , newleft | l1 | l2 = almost , nd z l1 l2 newleft c
  ... | almost , newleft | l1 | l2 = almost , nl z l1 l2 newleft c
  finsert z (nf p i j (nf x i₁ j₁ a b) c) | tri> _ _ z<p
    with finsert p (nf x i₁ j₁ a b)
    | lemma-<=minE (trans≤ (le (base z<p)) i) (le (base z<p))
  ... | full , newleft | l1 = almost , nd z l1 (trans≤ (le (base z<p)) j) newleft c
  ... | almost , newleft | l1 = almost ,
    nl z l1 (trans≤ (le (base z<p)) j) newleft c
\end{code}
Вставка элемента в неполную кучу.
\begin{code}
  ainsert : ∀ {h m} → (z : A) → Heap m h almost
    → Σ HeapState (Heap (minE m (# z)) h)
  ainsert z (nd p i j a b) with cmp p z
  ainsert z (nd p i j a b) | tri< p<z _ _
    with finsert z b | lemma-<=minE j (le (base p<z))
  ... | full   , nb | l1 = full , nf p i l1 a nb
  ... | almost , nb | l1 = almost , nr p i l1 a nb
  ainsert z (nd p i j a b) | tri= _ p=z _
    with finsert p b | snd resp≤ (base p=z) i
    | lemma-<=minE (snd resp≤ (base p=z) j) (eq (base (sym== p=z)))
  ... | full   , nb | l1 | l2 = full , nf z l1 l2 a nb
  ... | almost , nb | l1 | l2 = almost , nr z l1 l2 a nb
  ainsert z (nd p i j a b) | tri> _ _ z<p
    with finsert p b | trans≤ (le (base z<p)) i
    | lemma-<=minE (trans≤ (le (base z<p)) j) (le (base z<p))
  ... | full   , nb | l1 | l2 = full , nf z l1 l2 a nb
  ... | almost , nb | l1 | l2 = almost , nr z l1 l2 a nb
\end{code}
\begin{code}
  ainsert z (nl p i j a b) with cmp p z
  ainsert z (nl p i j a b) | tri< p<z _ _
    with ainsert z a | lemma-<=minE i (le (base p<z))
  ... | full , na | l1 = almost , nd p l1 j na b
  ... | almost , na | l1 = almost , nl p l1 j na b
  ainsert z (nl p i j a b) | tri= _ p=z _
    with ainsert p a | lemma-<=minE (snd resp≤ (base p=z) i)
      (eq (base (sym== p=z))) | snd resp≤ (base p=z) j
  ... | full , na | l1 | l2 = almost , nd z l1 l2 na b
  ... | almost , na | l1 | l2 = almost , nl z l1 l2 na b
  ainsert z (nl p i j a b) | tri> _ _ z<p
    with ainsert p a | lemma-<=minE (trans≤ (le (base z<p)) i)
      (le (base z<p)) | trans≤ (le (base z<p)) j
  ... | full , na | l1 | l2 = almost , nd z l1 l2 na b
  ... | almost , na | l1 | l2 = almost , nl z l1 l2 na b
  ainsert z (nr p i j a b) with cmp p z
  ainsert z (nr p i j a b) | tri< p<z _ _
    with ainsert z b | lemma-<=minE j (le (base p<z))
  ... | full , nb | l1 = full   , nf p i l1 a nb
  ... | almost , nb | l1 = almost , nr p i l1 a nb
  ainsert z (nr p i j a b) | tri= _ p=z _
    with ainsert p b | snd resp≤ (base p=z) i
    | lemma-<=minE (snd resp≤ (base p=z) j) (eq (base (sym== p=z)))
  ... | full , nb | l1 | l2 = full   , nf z l1 l2 a nb
  ... | almost , nb | l1 | l2 = almost , nr z l1 l2 a nb
  ainsert z (nr p i j a b) | tri> _ _ z<p
    with ainsert p b | trans≤ (le (base z<p)) i
    | lemma-<=minE (trans≤ (le (base z<p)) j) (le (base z<p))
  ... | full   , nb | l1 | l2 = full   , nf z l1 l2 a nb
  ... | almost , nb | l1 | l2 = almost , nr z l1 l2 a nb
\end{code}

\subsection{Удаление минимума из полной кучи}
Вспомогательный тип данных.
\begin{code}
  data OR (A B : Set) : Set where
    orA : A → OR A B
    orB : B → OR A B
\end{code} Слияние двух полных куч одной высоты.
\begin{code}
  fmerge : ∀ {x y h} → Heap x h full → Heap y h full
    → OR (Heap x zero full × (x ≡ y) × (h ≡ zero))
         (Heap (minE x y) (succ h) almost)
  fmerge eh eh = orA (eh , refl , refl)
  fmerge (nf x i₁ j₁ a b) (nf y i₂ j₂ c d) with cmp x y
  fmerge (nf x i₁ j₁ a b) (nf y i₂ j₂ c d) | tri< x<y _ _ with fmerge a b
  ... | orA (eh , refl , refl) = orB (nd x (le (base x<y)) j₁ (nf y i₂ j₂ c d) eh)
  ... | orB ab = orB
    (nr x (le (base x<y)) (lemma-<=minE i₁ j₁) (nf y i₂ j₂ c d) ab)
\end{code}
\begin{code}
  fmerge (nf x i₁ j₁ a b) (nf y i₂ j₂ c d) | tri= _ x=y _ with fmerge c d
  ... | orA (eh , refl , refl) = orB
    (nd y (eq (base (sym== x=y))) j₂ (nf x i₁ j₁ a b) eh)
  ... | orB cd = orB
    (nr y (eq (base (sym== x=y))) (lemma-<=minE i₂ j₂) (nf x i₁ j₁ a b) cd) 
\end{code}
\begin{code}
  fmerge (nf x i₁ j₁ a b) (nf y i₂ j₂ c d) | tri> _ _ y<x with fmerge c d
  ... | orA (eh , refl , refl) = orB (nd y (le (base y<x)) j₂ (nf x i₁ j₁ a b) eh)
  ... | orB cd = orB
    (nr y (le (base y<x)) (lemma-<=minE i₂ j₂) (nf x i₁ j₁ a b) cd)
\end{code}
Извлечение минимума из полной кучи.
\begin{code}
  fpop : ∀ {m h} → Heap m (succ h) full → OR
    (Σ (expanded A) (λ x → (Heap x (succ h) almost) × (m ≤ x)))
    (Heap top h full)

\end{code}
\begin{code}
  fpop (nf _ _ _ eh eh) = orB eh
  fpop (nf _ i j (nf x i₁ j₁ a b) (nf y i₂ j₂ c d))
    with fmerge (nf x i₁ j₁ a b) (nf y i₂ j₂ c d)
  ... | orA (() , _ , _)
  ... | orB res = orA ((minE (# x) (# y)) , res , lemma-<=minE i j)
\end{code}

\subsection{Удаление минимума из неполной кучи}
Составление полной кучи высотой $h+1$ из двух куч высотой $h$ и одного элемента.
\begin{code}
  makeH : ∀ {x y h} → (p : A) → Heap x h full → Heap y h full
    → Heap (min3E x y (# p)) (succ h) full
  makeH p eh eh = nf p (le ext) (le ext) eh eh
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) with cmp x y
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri< x<y _ _ with cmp x p
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri< x<y _ _ | tri< x<p _ _
    with makeH p a b
  ... | res = nf x (lemma-<=min3E i j (le (base x<p))) (le (base x<y))
    res (nf y i₁ j₁ c d)
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri< x<y _ _ | tri= _ x=p _ =
    nf p (eq (base (sym== x=p))) (le (base (snd resp x=p x<y)))
      (nf x i j a b) (nf y i₁ j₁ c d)
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri< x<y _ _ | tri> _ _ p<x =
    nf p (le (base p<x)) (le (base (trans< p<x x<y)))
      (nf x i j a b) (nf y i₁ j₁ c d)
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ with cmp y p
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | tri< y<p _ _ =
    nf y (eq (base (sym== x=y))) (lemma-<=min3E i₁ j₁ (le (base y<p)))
      (nf x i j a b) (makeH p c d)
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | tri= _ y=p _ =
    nf p (eq (base (trans== (sym== y=p) (sym== x=y))))
      (eq (base (sym== y=p))) (nf x i j a b) (nf y i₁ j₁ c d)
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | tri> _ _ p<y =
    nf p (le (base (fst resp (sym== x=y) p<y))) (le (base p<y))
      (nf x i j a b) (nf y i₁ j₁ c d)
\end{code}
\begin{code}
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri> _ _ y<x with cmp y p
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri> _ _ y<x | tri< y<p _ _ =
    nf y (le (base y<x)) (lemma-<=min3E i₁ j₁ (le (base y<p)))
      (nf x i j a b) (makeH p c d)
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri> _ _ y<x | tri= _ y=p _ =
    nf p (le (base (snd resp y=p y<x))) (eq (base (sym== y=p)))
      (nf x i j a b) (nf y i₁ j₁ c d)
  makeH p (nf x i j a b) (nf y i₁ j₁ c d) | tri> _ _ y<x | tri> _ _ p<y =
    nf p (le (base (trans< p<y y<x))) (le (base p<y))
      (nf x i j a b) (nf y i₁ j₁ c d)
\end{code} Вспомогательные леммы, использующие \F{lemma-<=minE}.
\begin{code}
  lemma-resp : ∀ {x y a b} → x == y → (# x) ≤ a → (# x) ≤ b
    → (# y) ≤ minE a b
  lemma-resp x=y i j = lemma-<=minE (snd resp≤ (base x=y) i)
    (snd resp≤ (base x=y) j)
  lemma-trans : ∀ {x y a b} → y < x → (# x) ≤ a → (# x) ≤ b
    → (# y) ≤ minE a b
  lemma-trans y<x i j = lemma-<=minE (trans≤ (le (base y<x)) i)
    (trans≤ (le (base y<x)) j)
\end{code} Слияние поддеревьев из кучи, у которой последний ряд заполнен до середины,
определенной конструктором \DC{nd}.
\begin{code}
  ndmerge : ∀ {x y h} → Heap x (succ (succ h)) full → Heap y (succ h) full
    → Heap (minE x y) (succ (succ (succ h))) almost

  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) with cmp x y
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri< x<y _ _ with fmerge a b
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri< x<y _ _ | orA (_ , _ , ())
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri< x<y _ _ | orB x₁ =
    nl x (lemma-<=minE i j) (le (base x<y)) x₁ (nf y i₁ j₁ c d)

  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ with fmerge c d
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | orA (eh , refl , refl)
    with fmerge a b
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | orA (eh , refl , refl)
    | orA (eh , refl , ())
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | orA (eh , refl , refl)
    | orB ab = nl y (lemma-resp x=y i j) (eq (base (sym== x=y)))
                 ab (nf x (le ext) (le ext) eh eh)

  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | orB cd with fmerge a b
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | orB cd | orA (_ , _ , ())
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri= _ x=y _ | orB cd | orB ab =
    nl y (lemma-resp x=y i j) (lemma-<=min3E i₁ j₁ (eq (base (sym== x=y))))
      ab (makeH x c d)
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri> _ _ y<x with fmerge a b
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri> _ _ y<x | orA (_ , _ , ())
  ndmerge (nf x i j a b) (nf y i₁ j₁ c d) | tri> _ _ y<x | orB ab =
    nl y (lemma-trans y<x i j) (lemma-<=min3E i₁ j₁ (le (base y<x)))
      ab (makeH x c d)
\end{code} Слияние неполной кучи высотой $h+2$ и полной кучи высотой $h+1$ или $h+2$.
\begin{code}
  afmerge : ∀ {h x y} → Heap x (succ (succ h)) almost
    → OR (Heap y (succ h) full) (Heap y (succ (succ h)) full)
    → OR (Heap (minE x y) (succ (succ h)) full)
         (Heap (minE x y) (succ (succ (succ h))) almost)

  afmerge (nd x i j (nf p i₁ j₁ eh eh) eh) (orA (nf y i₂ j₂ eh eh)) with cmp x y
  ... | tri< x<y _ _ = orA (nf x i (le (base x<y))
    (nf p i₁ j₁ eh eh) (nf y i₂ j₂ eh eh))
  ... | tri= _ x=y _ = orA (nf y (eq (base (sym== x=y)))
    (snd resp≤ (base x=y) i) (nf x (le ext) (le ext) eh eh) (nf p i₁ j₁ eh eh))
  ... | tri> _ _ y<x = orA (nf y (le (base y<x))
    (trans≤ (le (base y<x)) i) (nf x j j eh eh) (nf p j₁ j₁ eh eh))

  afmerge (nd x i j (nf p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orA (nf y i₃ j₃ c d))
    with cmp x y | ndmerge (nf p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)
  ... | tri< x<y _ _ | ab = orB (nl x (lemma-<=minE i j) (le (base x<y))
    ab (nf y i₃ j₃ c d))
  ... | tri= _ x=y _ | ab = orB (nl y (lemma-resp x=y i j)
    (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri> _ _ y<x | ab = orB (nl y (lemma-trans y<x i j)
    (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))
  afmerge (nl x i j (nd p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orA (nf y i₃ j₃ c d))
    with cmp x y | afmerge (nd p₁ i₁ j₁ a₁ b₁) (orA (nf p₂ i₂ j₂ a₂ b₂))
  ... | tri< x<y _ _ | orA ab =
    orA (nf x (lemma-<=minE i j) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri< x<y _ _ | orB ab =
    orB (nl x (lemma-<=minE i j) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri= _ x=y _ | orA ab = orA
    (nf y (lemma-resp x=y i j) (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y))))
      ab (makeH x c d))
  ... | tri= _ x=y _ | orB ab = orB
    (nl y (lemma-resp x=y i j) (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y))))
      ab (makeH x c d))
  ... | tri> _ _ y<x | orA ab = orA
    (nf y (lemma-trans y<x i j) (lemma-<=min3E i₃ j₃ (le (base y<x)))
      ab (makeH x c d))
  ... | tri> _ _ y<x | orB ab = orB
    (nl y (lemma-trans y<x i j) (lemma-<=min3E i₃ j₃ (le (base y<x)))
      ab (makeH x c d))

  afmerge (nl x i j (nl p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orA (nf y i₃ j₃ c d))
    with cmp x y | afmerge (nl p₁ i₁ j₁ a₁ b₁) (orA (nf p₂ i₂ j₂ a₂ b₂))
  ... | tri< x<y _ _ | orA ab =
    orA (nf x (lemma-<=minE i j) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri< x<y _ _ | orB ab =
    orB (nl x (lemma-<=minE i j) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri= _ x=y _ | orA ab = orA (nf y (lemma-resp x=y i j)
    (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri= _ x=y _ | orB ab = orB (nl y (lemma-resp x=y i j)
    (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri> _ _ y<x | orA ab = orA (nf y (lemma-trans y<x i j)
    (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))
  ... | tri> _ _ y<x | orB ab = orB (nl y (lemma-trans y<x i j)
    (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))

  afmerge (nl x i j (nr p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orA (nf y i₃ j₃ c d))
    with cmp x y | afmerge (nr p₁ i₁ j₁ a₁ b₁) (orA (nf p₂ i₂ j₂ a₂ b₂))
  ... | tri< x<y _ _ | orA ab =
    orA (nf x (lemma-<=minE i j) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri< x<y _ _ | orB ab =
      orB (nl x (lemma-<=minE i j) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri= _ x=y _ | orA ab = orA (nf y (lemma-resp x=y i j)
    (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri= _ x=y _ | orB ab = orB (nl y (lemma-resp x=y i j)
    (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri> _ _ y<x | orA ab = orA (nf y (lemma-trans y<x i j)
    (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))
  ... | tri> _ _ y<x | orB ab = orB (nl y (lemma-trans y<x i j)
    (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))

  afmerge (nr x i j (nf p₁ i₁ j₁ a₁ b₁) (nd p₂ i₂ j₂ a₂ b₂)) (orA (nf y i₃ j₃ c d))
    with cmp x y | afmerge (nd p₂ i₂ j₂ a₂ b₂) (orB (nf p₁ i₁ j₁ a₁ b₁))
  ... | tri< x<y _ _ | (orA ab) =
    orA (nf x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab)
  ... | tri< x<y _ _ | (orB ab) =
    orB (nl x (lemma-<=minE j i) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri= _ x=y _ | (orA ab) = orA (nf y (lemma-resp x=y j i)
    (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri= _ x=y _ | (orB ab) = orB (nl y (lemma-resp x=y j i)
    (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri> _ _ y<x | (orA ab) = orA (nf y (lemma-trans y<x j i)
    (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))
  ... | tri> _ _ y<x | (orB ab) = orB (nl y (lemma-trans y<x j i)
    (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))

  afmerge (nr x i j (nf p₁ i₁ j₁ a₁ b₁) (nl p₂ i₂ j₂ a₂ b₂)) (orA (nf y i₃ j₃ c d))
    with cmp x y | afmerge (nl p₂ i₂ j₂ a₂ b₂) (orB (nf p₁ i₁ j₁ a₁ b₁))
  ... | tri< x<y _ _ | (orA ab) =
    orA (nf x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab)
  ... | tri< x<y _ _ | (orB ab) =
    orB (nl x (lemma-<=minE j i) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri= _ x=y _ | (orA ab) = orA (nf y (lemma-resp x=y j i)
    (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri= _ x=y _ | (orB ab) = orB (nl y (lemma-resp x=y j i)
    (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri> _ _ y<x | (orA ab) = orA (nf y (lemma-trans y<x j i)
    (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))
  ... | tri> _ _ y<x | (orB ab) = orB (nl y (lemma-trans y<x j i)
    (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))

  afmerge (nr x i j (nf p₁ i₁ j₁ a₁ b₁) (nr p₂ i₂ j₂ a₂ b₂)) (orA (nf y i₃ j₃ c d))
    with cmp x y | afmerge (nr p₂ i₂ j₂ a₂ b₂) (orB (nf p₁ i₁ j₁ a₁ b₁))
  ... | tri< x<y _ _ | (orA ab) =
    orA (nf x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab)
  ... | tri< x<y _ _ | (orB ab) =
    orB (nl x (lemma-<=minE j i) (le (base x<y)) ab (nf y i₃ j₃ c d))
  ... | tri= _ x=y _ | (orA ab) = orA (nf y (lemma-resp x=y j i)
    (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri= _ x=y _ | (orB ab) = orB (nl y (lemma-resp x=y j i)
    (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) ab (makeH x c d))
  ... | tri> _ _ y<x | (orA ab) = orA (nf y (lemma-trans y<x j i)
    (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))
  ... | tri> _ _ y<x | (orB ab) = orB (nl y (lemma-trans y<x j i)
    (lemma-<=min3E i₃ j₃ (le (base y<x))) ab (makeH x c d))

  afmerge (nd x i j (nf p i₁ j₁ eh eh) eh) (orB (nf y i₂ j₂ c d)) with cmp x y
  ... | tri< x<y _ _ =
    orB (nd x (le (base x<y)) i (nf y i₂ j₂ c d) (nf p i₁ j₁ eh eh))
  ... | tri= _ x=y _ = orB (nd y
     (lemma-<=min3E i₂ j₂ (eq (base (sym== x=y)))) (snd resp≤ (base x=y) i)
     (makeH x c d) (nf p i₁ j₁ eh eh))
  ... | tri> _ _ y<x = orB (nd y (lemma-<=min3E i₂ j₂ (le (base y<x)))
    (trans≤ (le (base y<x)) i) (makeH x c d) (nf p i₁ j₁ eh eh))

  afmerge (nd x i j (nf p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orB (nf y i₃ j₃ c d))
    with cmp x y | ndmerge (nf p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)
  ... | tri< x<y _ _ | ab =
    orB (nr x (le (base x<y)) (lemma-<=minE i j) (nf y i₃ j₃ c d) ab)
  ... | tri= _ x=y _ | ab = orB (nr y
    (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y))))
    (lemma-resp x=y i j) (makeH x c d) ab)
  ... | tri> _ _ y<x | ab = orB (nr y
    (lemma-<=min3E i₃ j₃ (le (base y<x)))
    (lemma-trans y<x i j) (makeH x c d) ab)

  afmerge (nl x i j (nd p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orB (nf y i₃ j₃ c d))
    with cmp x y | afmerge (nd p₁ i₁ j₁ a₁ b₁) (orA (nf p₂ i₂ j₂ a₂ b₂))
  ... | tri< x<y _ _ | (orA ab) = orB (nd x (le (base x<y))
    (lemma-<=minE i j) (nf y i₃ j₃ c d) ab)
  ... | tri< x<y _ _ | (orB ab) = orB (nr x (le (base x<y))
    (lemma-<=minE i j) (nf y i₃ j₃ c d) ab)
  ... | tri= _ x=y _ | (orA ab) = orB (nd y
    (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y i j)
      (makeH x c d) ab)
  ... | tri= _ x=y _ | (orB ab) = orB (nr y
    (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y i j)
      (makeH x c d) ab)
  ... | tri> _ _ y<x | (orA ab) = orB (nd y
    (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x i j) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orB ab) = orB (nr y
    (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x i j) (makeH x c d) ab)
  afmerge (nl x i j (nl p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orB (nf y i₃ j₃ c d))
    with cmp x y | afmerge (nl p₁ i₁ j₁ a₁ b₁) (orA (nf p₂ i₂ j₂ a₂ b₂))
  ... | tri< x<y _ _ | (orA ab) = orB (nd x (le (base x<y))
    (lemma-<=minE i j) (nf y i₃ j₃ c d) ab)
  ... | tri< x<y _ _ | (orB ab) = orB (nr x (le (base x<y))
    (lemma-<=minE i j) (nf y i₃ j₃ c d) ab)
  ... | tri= _ x=y _ | (orA ab) = orB
    (nd y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y))))
    (lemma-resp x=y i j) (makeH x c d) ab)
  ... | tri= _ x=y _ | (orB ab) = orB
    (nr y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y))))
    (lemma-resp x=y i j) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orA ab) = orB
    (nd y (lemma-<=min3E i₃ j₃ (le (base y<x)))
    (lemma-trans y<x i j) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orB ab) = orB
    (nr y (lemma-<=min3E i₃ j₃ (le (base y<x)))
    (lemma-trans y<x i j) (makeH x c d) ab)

  afmerge (nl x i j (nr p₁ i₁ j₁ a₁ b₁) (nf p₂ i₂ j₂ a₂ b₂)) (orB (nf y i₃ j₃ c d))
    with cmp x y | afmerge (nr p₁ i₁ j₁ a₁ b₁) (orA (nf p₂ i₂ j₂ a₂ b₂))
  ... | tri< x<y _ _ | (orA ab) = orB
    (nd x (le (base x<y)) (lemma-<=minE i j) (nf y i₃ j₃ c d) ab)
  ... | tri< x<y _ _ | (orB ab) = orB
    (nr x (le (base x<y)) (lemma-<=minE i j) (nf y i₃ j₃ c d) ab)
  ... | tri= _ x=y _ | (orA ab) = orB
    (nd y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y))))
    (lemma-resp x=y i j) (makeH x c d) ab)
  ... | tri= _ x=y _ | (orB ab) = orB
    (nr y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y))))
    (lemma-resp x=y i j) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orA ab) = orB
    (nd y (lemma-<=min3E i₃ j₃ (le (base y<x)))
    (lemma-trans y<x i j) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orB ab) = orB
    (nr y (lemma-<=min3E i₃ j₃ (le (base y<x)))
    (lemma-trans y<x i j) (makeH x c d) ab)

  afmerge (nr x i j (nf p₁ i₁ j₁ a₁ b₁) (nd p₂ i₂ j₂ a₂ b₂)) (orB (nf y i₃ j₃ c d))
    with cmp x y | afmerge (nd p₂ i₂ j₂ a₂ b₂) (orB (nf p₁ i₁ j₁ a₁ b₁)) 
  ... | tri< x<y _ _ | (orA ab) = orB
    (nd x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab) 
  ... | tri< x<y _ _ | (orB ab) = orB
    (nr x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab)
  ... | tri= _ x=y _ | (orA ab) = orB
    (nd y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y))))
    (lemma-resp x=y j i) (makeH x c d) ab)
  ... | tri= _ x=y _ | (orB ab) = orB
    (nr y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y))))
    (lemma-resp x=y j i) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orA ab) = orB
    (nd y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x j i)
      (makeH x c d) ab)
  ... | tri> _ _ y<x | (orB ab) = orB
    (nr y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x j i)
      (makeH x c d) ab)
  afmerge (nr x i j (nf p₁ i₁ j₁ a₁ b₁) (nl p₂ i₂ j₂ a₂ b₂)) (orB (nf y i₃ j₃ c d))
    with cmp x y | afmerge (nl p₂ i₂ j₂ a₂ b₂) (orB (nf p₁ i₁ j₁ a₁ b₁)) 
  ... | tri< x<y _ _ | (orA ab) = orB
    (nd x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab) 
  ... | tri< x<y _ _ | (orB ab) = orB
    (nr x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab)
  ... | tri= _ x=y _ | (orA ab) = orB
    (nd y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y j i)
      (makeH x c d) ab)
  ... | tri= _ x=y _ | (orB ab) = orB
    (nr y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y)))) (lemma-resp x=y j i)
      (makeH x c d) ab)
  ... | tri> _ _ y<x | (orA ab) = orB
    (nd y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x j i)
      (makeH x c d) ab)
  ... | tri> _ _ y<x | (orB ab) = orB
    (nr y (lemma-<=min3E i₃ j₃ (le (base y<x))) (lemma-trans y<x j i)
      (makeH x c d) ab)
  afmerge (nr x i j (nf p₁ i₁ j₁ a₁ b₁) (nr p₂ i₂ j₂ a₂ b₂)) (orB (nf y i₃ j₃ c d))
    with cmp x y | afmerge (nr p₂ i₂ j₂ a₂ b₂) (orB (nf p₁ i₁ j₁ a₁ b₁)) 
  ... | tri< x<y _ _ | (orA ab) = orB
    (nd x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab) 
  ... | tri< x<y _ _ | (orB ab) = orB
    (nr x (le (base x<y)) (lemma-<=minE j i) (nf y i₃ j₃ c d) ab)
  ... | tri= _ x=y _ | (orA ab) = orB
    (nd y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y))))
    (lemma-resp x=y j i) (makeH x c d) ab)
  ... | tri= _ x=y _ | (orB ab) = orB
    (nr y (lemma-<=min3E i₃ j₃ (eq (base (sym== x=y))))
    (lemma-resp x=y j i) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orA ab) = orB
    (nd y (lemma-<=min3E i₃ j₃ (le (base y<x)))
    (lemma-trans y<x j i) (makeH x c d) ab)
  ... | tri> _ _ y<x | (orB ab) = orB
    (nr y (lemma-<=min3E i₃ j₃ (le (base y<x)))
    (lemma-trans y<x j i) (makeH x c d) ab)
\end{code} Извлечение минимума из неполной кучи.
\begin{code}
  apop : ∀ {m h} → Heap m (succ h) almost
    → OR (Σ (expanded A) (λ x → (Heap x (succ h) almost) × (m ≤ x)))
         (Σ (expanded A) (λ x → (Heap x h full) × (m ≤ x)))

  apop (nd {x = x} p i j a eh) = orB (x , a , i)
  apop (nd _ i j (nf x i₁ j₁ a b) (nf y i₂ j₂ c d))
    with cmp x y | ndmerge (nf x i₁ j₁ a b) (nf y i₂ j₂ c d)
  ... | tri< _ _ _ | res = orA (# x , res , i)
  ... | tri= _ _ _ | res = orA (# y , res , j)
  ... | tri> _ _ _ | res = orA (# y , res , j)
  apop (nl _ i j (nd x i₁ j₁ (nf y _ _ eh eh) eh) (nf z _ _ eh eh))
    with cmp x z
  ... | tri< x<z _ _ = orB (# x , nf x i₁ (le (base x<z))
    (nf y (le ext) (le ext) eh eh) (nf z (le ext) (le ext) eh eh) , i)
  ... | tri= _ x=z _ = orB (# z ,
    nf z (eq (base (sym== x=z))) (snd resp≤ (base x=z) i₁)
      (nf x (le ext) (le ext) eh eh) (nf y (le ext) (le ext) eh eh) , j)
  ... | tri> _ _ z<x = orB (# z , nf z
    (le (base z<x)) (trans≤ (le (base z<x)) i₁)
    (nf x (le ext) (le ext) eh eh) (nf y (le ext) (le ext) eh eh) , j)

  apop (nl _ i j (nd x i₁ j₁ (nf y i₂ j₂ a₂ b₂) (nf z i₃ j₃ a₃ b₃)) (nf t i₄ j₄ c d))
    with cmp x t | ndmerge (nf y i₂ j₂ a₂ b₂) (nf z i₃ j₃ a₃ b₃)
  ... | tri< x<t _ _ | res = orA (# x , nl x
    (lemma-<=minE i₁ j₁) (le (base x<t))
    res (nf t i₄ j₄ c d) , i)
  ... | tri= _ x=t _ | res = orA (# t , nl t
    (snd resp≤ (base x=t) (lemma-<=minE i₁ j₁))
    (lemma-<=min3E i₄ j₄ (eq (base (sym== x=t)))) res (makeH x c d) , j)
  ... | tri> _ _ t<x | res = orA (# t , nl t
    (lemma-trans t<x i₁ j₁)
    (lemma-<=min3E i₄ j₄ (le (base t<x))) res (makeH x c d) , j)

  apop (nl _ i j (nl x i₁ j₁ a b) (nf y i₂ j₂ c d))
    with cmp x y | afmerge (nl x i₁ j₁ a b) (orA (nf y i₂ j₂ c d))
  ... | tri< _ _ _ | orA res = orB (# x , res , i)
  ... | tri= _ _ _ | orA res = orB (# y , res , j)
  ... | tri> _ _ _ | orA res = orB (# y , res , j)
  ... | tri< _ _ _ | orB res = orA (# x , res , i)
  ... | tri= _ _ _ | orB res = orA (# y , res , j)
  ... | tri> _ _ _ | orB res = orA (# y , res , j)
  apop (nl _ i j (nr x i₁ j₁ a b) (nf y i₂ j₂ c d))
    with cmp x y | afmerge (nr x i₁ j₁ a b) (orA (nf y i₂ j₂ c d))
  ... | tri< _ _ _ | orA res = orB (# x , res , i)
  ... | tri= _ _ _ | orA res = orB (# y , res , j)
  ... | tri> _ _ _ | orA res = orB (# y , res , j)
  ... | tri< _ _ _ | orB res = orA (# x , res , i)
  ... | tri= _ _ _ | orB res = orA (# y , res , j)
  ... | tri> _ _ _ | orB res = orA (# y , res , j)
  apop (nr _ i j (nf x i₁ j₁ a b) (nd y i₂ j₂ c d))
    with cmp y x | afmerge (nd y i₂ j₂ c d) (orB (nf x i₁ j₁ a b))
  ... | tri< _ _ _ | orA res = orB (# y , res , j)
  ... | tri= _ _ _ | orA res = orB (# x , res , i)
  ... | tri> _ _ _ | orA res = orB (# x , res , i)
  ... | tri< _ _ _ | orB res = orA (# y , res , j)
  ... | tri= _ _ _ | orB res = orA (# x , res , i)
  ... | tri> _ _ _ | orB res = orA (# x , res , i)
  apop (nr _ i j (nf x i₁ j₁ a b) (nl y i₂ j₂ c d))
    with cmp y x | afmerge (nl y i₂ j₂ c d) (orB (nf x i₁ j₁ a b))
  ... | tri< _ _ _ | orA res = orB (# y , res , j)
  ... | tri= _ _ _ | orA res = orB (# x , res , i)
  ... | tri> _ _ _ | orA res = orB (# x , res , i)
  ... | tri< _ _ _ | orB res = orA (# y , res , j)
  ... | tri= _ _ _ | orB res = orA (# x , res , i)
  ... | tri> _ _ _ | orB res = orA (# x , res , i)
  apop (nr _ i j (nf x i₁ j₁ a b) (nr y i₂ j₂ c d))
    with cmp y x | afmerge (nr y i₂ j₂ c d) (orB (nf x i₁ j₁ a b))
  ... | tri< _ _ _ | orA res = orB (# y , res , j)
  ... | tri= _ _ _ | orA res = orB (# x , res , i)
  ... | tri> _ _ _ | orA res = orB (# x , res , i)
  ... | tri< _ _ _ | orB res = orA (# y , res , j)
  ... | tri= _ _ _ | orB res = orA (# x , res , i)
  ... | tri> _ _ _ | orB res = orA (# x , res , i)

\end{code}

