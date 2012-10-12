{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- В данном задании требуется реализовать интерпретатор для
-- нетипизированной лямбды
--------------------------------------------------------------------------------

module UnTyLambda.Interpreter where

-- Какие-то импорты. Заметьте, что в этом задании надо
-- использовать обычную Prelude
import Prelude hiding (catch)
import Control.Exception
import Data.List
------------------------------------------------------------
-- Определение дататайпа для нетипизированной лямбды
type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

------------------------------------------------------------
-- Дальше всё на ваше усмотрение

-- Если внутри будете использовать именованное представление, то
-- я тут решил немного вам помочь
-- (иначе говоря, код из этого раздела можно совсем выкинуть,
-- если хочется)

free (Var v)    = [ v ]
free (Lam v t)  = filter (/= v) . free $ t
free (App t t') = (free t) `union` (free t')

subst :: Term -> Variable -> Term -> Term
subst t@(Var v)   var what = if v == var then what else t
subst t@(Lam v b) var what = if v == var then t else Lam v (subst b var what)
subst (App t t')  var what = App (subst t var what) (subst t' var what)

newname fv = head . filter (not . flip elem fv) . iterate (++ "\'")

renameVars :: [Variable] -> Term -> Term
renameVars vs (Var v) = Var $ newname vs v
renameVars vs (Lam x t) = let x' = newname (vs `union` free t) x in Lam x' $ subst t x (Var x')
renameVars vs (App a b) = App (renameVars vs a) (renameVars vs b)
--- ...

------------------------------------------------------------
-- За исключением того, что требуется реализовать следующие
-- стратегии нормализации (они все принимают максимальное
-- число шагов интерпретатора в качестве первого 
-- параметра (n); если за n шагов нормализовать не удаётся,
-- то следует бросать error, тестер его поймает):

wh, no, wa, sa :: Integer -> Term -> Term

reduceError t = error $ "Too long sequence at [" ++ show t ++ "]"

-- Редукция аппликативным порядком
sa 0 t = reduceError t 
sa n t = if wasReduced
            then sa (n-1) t'
            else t
    where
    (wasReduced, t') = saTryReduce t

saTryReduce :: Term -> (Bool, Term)
saTryReduce t@(App f x) = if xreduced
    then (True, App f x')
    else case f of
        (Lam a b) -> (True, subst b' a x) where
            b' = renameVars (free x) $ snd $ saTryReduce b
        _         -> (freduced, App f' x)
    where
        (xreduced, x') = saTryReduce x
        (freduced, f') = saTryReduce f
saTryReduce t = (False, t)

-- Нормализация нормальным порядком
no 0 t = reduceError t
no n t = if wasReduced
            then no (n-1) t'
            else t
    where
    (wasReduced, t') = noTryReduce t

noTryReduce :: Term -> (Bool, Term)
noTryReduce (Lam a b) = (breduced, Lam a b')
    where (breduced, b') = noTryReduce b
noTryReduce (App (Lam a b) x) = (True, subst (renameVars (free x) b) a x)
noTryReduce (App f x) = (freduced || xreduced, App f' x')
    where
        (freduced, f') = noTryReduce f
        (xreduced, x') = noTryReduce x
noTryReduce t = (False, t)

-- Редукция в слабую головную нормальную форму
wh 0 t = reduceError t
wh n t = if wasReduced
            then wh (n-1) t'
            else t
    where
    (wasReduced, t') = whTryReduce t

whTryReduce :: Term -> (Bool, Term)
whTryReduce (App (Lam a b) x) = (True, subst (renameVars (free x) b) a x)
whTryReduce (App f x) = (freduced || xreduced, App f' x')
    where
        (freduced, f') = whTryReduce f
        (xreduced, x') = whTryReduce x
whTryReduce t = (False, t)

-- (*) (не обязательно) Редукция "слабым" аппликативным порядком.
-- Отличается от обычного аппликативного тем, что не лезет внутрь
-- лямбд и правые части аппликаций, когда это возможно.
wa = undefined

-- Замечание: cкорость работы вашего интерпретатора специально не оценивается,
-- потому можно использовать свой изоморфный (с точностью до альфа-конверсии)
-- тип для представления термов и преобразовывать Term в него и обратно.

-- Перечисление всех этих порядков (в порядке отличном от
-- определения, да)
orders =
    [ ("wh", wh)
    , ("no", no)
--    , ("wa", wa) -- Можно раскоментировать, да
    , ("sa", sa) ]

------------------------------------------------------------
-- Игнорируйте это, если выглядит непонятно
pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

------------------------------------------------------------
-- Сюда можно добавлять тесты
lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

test = testfuncs orders
    [ Var "a"
    , Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
    , (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
    , omega
    ]

------------------------------------------------------------
-- Немного теоретических замечаний, если они вас волнуют
--
-- Следует специально отметить, что поскольку в конце теста
-- результат вычисления печатают, то ленивость Haskell не
-- влияет на семантику интерпретируемого исчисления.
--
-- Чтобы это особенно подчеркнуть в тестере выше я написал
-- seq в интересном месте (хотя конкретно это там ничего не
-- гарантирует, на самом-то деле).
