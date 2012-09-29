{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil | Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length a = f a Zero where
    f Nil l = l
    f (Cons _ t) l = f t (Succ l)

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ b = b
(Cons a t) ++ b = Cons a $ t ++ b

-- Список без первого элемента
tail :: List a -> List a
tail Nil = error "no tail in empty list"
tail (Cons _ t) = t

-- Список без последнего элемента
init :: List a -> List a
init Nil = error "no init in empty list"
init (Cons _ Nil) = Nil
init (Cons a t)   = Cons a $ init t
-- Первый элемент
head :: List a -> a
head Nil = error "no head in empty list"
head (Cons a _) = a

-- Последний элемент
last :: List a -> a
last Nil = error "no last element in empty list"
last (Cons a Nil) = a
last (Cons _ t) = last t

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero _   = Nil
take n    Nil = error "nothing to take in empty list"
take (Succ n) (Cons a t) = Cons a $ take n t

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop _ Nil  = Nil
drop Zero a = a
drop (Succ n) (Cons a t) = drop n t

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons a t) = if' (p a)
    (Cons a $ filter p t) (filter p t)

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter _ Nil = Nil
gfilter p (Cons a t) = case p a of
    Nothing -> gfilter p t
    Just b  -> Cons b $ gfilter p t

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile p (Cons a t) = if' (p a) (Cons a $ takeWhile p t) Nil

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile p a@(Cons b t) = if' (p b) (dropWhile p t) a

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span _ Nil = Pair Nil Nil
span p l@(Cons a t) = if' (p a)
    (Pair (Cons a $ fst t') (snd t'))
    (Pair Nil l) where
        t' = span p t 

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p = span (not . p)

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons a _) !! Zero = a
(Cons _ t) !! (Succ n) = t !! n

-- Список задом на перёд
reverse :: List a -> List a
reverse a = f a Nil where
    f Nil a = a
    f (Cons a t) b = f t $ Cons a b

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons a t) = (f a st) ++ st where
    st = subsequences t
    f a (Cons b st') = Cons (Cons a b) $ f a st'
    f a Nil = Nil

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations Nil = Cons Nil Nil
permutations (Cons a t) = i a $ permutations t where
    i a Nil = Nil
    i a (Cons l ls) = (i' a l) ++ (i a ls) where
        i' a Nil = Cons (Cons a Nil) Nil
        i' a (Cons b t) = (Cons (Cons a (Cons b t)) Nil) ++
            map (Cons b) (i' a t) 
-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = Cons a $ repeat a

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl _ s Nil = s
foldl f s (Cons a t) = foldl f (f s a) t

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl _ s Nil = Cons s Nil
scanl f s (Cons a t) = Cons s $ scanl f (f s a) t
-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr _ s Nil = s
foldr f s (Cons a t) = f a $ foldr f s t 

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr _ s Nil = Cons s Nil
scanr f s (Cons a t) = Cons (f a $ head t') t' where
    t' = scanr f s t

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons a t) = Cons (f a) $ map f t

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat = foldr (++) Nil

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap _ Nil = Nil
concatMap f (Cons a t) = (f a) ++ (concatMap f t)

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip _ Nil = Nil
zip Nil _ = Nil
zip (Cons a t) (Cons b t') = Cons (Pair a b) $ zip t t'


-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith _ _ Nil = Nil
zipWith _ Nil _ = Nil
zipWith f (Cons a t) (Cons b t') = Cons (f a b) $ zipWith f t t'
