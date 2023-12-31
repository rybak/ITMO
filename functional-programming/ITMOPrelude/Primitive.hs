{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read,show)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero			= EQ
natCmp Zero _       	    = LT
natCmp _ Zero               = GT
natCmp (Succ n) (Succ m)	= natCmp n m

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq n m = case (natCmp n m) of
    EQ -> True
    _  -> False

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt n m = case (natCmp n m) of
    LT -> True
    _  -> False 

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
--n -. m = undefined
Zero -. _           = Zero
(Succ n) -. Zero    = Succ n
(Succ n) -. (Succ m)= n -. m

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n
-- Целое и остаток от деления n на m


-- divByZero = error "Division by zero"

natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n m = if' (n `natLt` m) (Pair Zero n)
    (Pair (Succ $ fst predDivMod)(snd predDivMod)) where
    predDivMod = natDivMod (n -. m) m

natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd n Zero = n
gcd n m    = gcd m $ natMod n m
-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Pos Nat | Neg Nat deriving (Show,Read)
intZero   = Pos Zero-- 0
intOne    = Pos $ Succ Zero -- natOne -- 1
intNegOne = Neg Zero-- -1

-- n -> - n
intNeg :: Int -> Int
intNeg (Pos Zero)     = intZero
intNeg (Pos (Succ n)) = Neg n 
intNeg (Neg n)        = Pos $ Succ n 

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp (Neg _) (Pos _) = LT
intCmp (Pos _) (Neg _) = GT
intCmp (Pos n) (Pos m) = natCmp n m
intCmp (Neg n) (Neg m) = natCmp n m

intEq :: Int -> Int -> Bool
intEq n m = case (intCmp n m) of
    EQ -> True
    _  -> False 

intLt :: Int -> Int -> Bool
intLt n m = case (intCmp n m) of
    LT -> True
    _  -> False 

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
(Pos n) .+. (Pos m) = Pos $ m +. n --  :-)
(Neg n) .+. (Neg m) = Neg $ Succ $ m +. n
(Pos n) .+. (Neg m) = if' (m `natLt` n) (Pos $ pred $ n -. m) (Neg $ m -. n)
a@(Neg _) .+. b@(Pos _) = b .+. a

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
(Pos n) .*. (Pos m) = Pos $ n *. m
(Neg n) .*. (Neg m) = Pos $ (Succ n) *. (Succ m)
a@(Pos _) .*. b@(Neg _) = intNeg $ a .*. (intNeg b) 
a@(Neg _) .*. b@(Pos _) = b .*. a

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat

ratRed :: Rat -> Rat
ratRed (Rat (Pos Zero) _) = Rat (Pos Zero) (Succ Zero)
ratRed (Rat (Pos n) m) = Rat (Pos (n `natDiv` nm)) (m `natDiv` nm) where
    nm = gcd n m
ratRed p@(Rat (Neg _) _)  = ratNeg $ ratRed $ ratNeg p

ratNeg :: Rat -> Rat
ratNeg (Rat a n) = Rat (intNeg a) n

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat (Pos Zero) _) = undefined
ratInv (Rat (Pos n) m) = Rat (Pos m) n
ratInv p@(Rat (Neg _) _) = ratNeg $ ratInv $ ratNeg p

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat a n) (Rat b m) = intCmp (a .*. (Pos m)) (b .*. (Pos n)) 

ratEq :: Rat -> Rat -> Bool
ratEq p q = case (ratCmp p q) of
    EQ -> True
    _  -> False

ratLt :: Rat -> Rat -> Bool
ratLt p q = case (ratCmp p q) of
    LT -> True
    _  -> False

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat a n) %+ (Rat b m) = ratRed $
    Rat (a .*. (Pos m) .+. b .*. (Pos n)) (n *. m)

(%-) :: Rat -> Rat -> Rat
p %- q = p %+ (ratNeg q)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat a n) %* (Rat b m) = ratRed $ Rat (a .*. b) (n *. m) 

(%/) :: Rat -> Rat -> Rat
p %/ q = p %* (ratInv q)

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
