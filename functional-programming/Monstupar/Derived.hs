module Monstupar.Derived where

-- В этом модуле не видно структуры типа Monstupar из
import Monstupar.Core
-- например,
-- blah = Monstupar $ undefined
-- не скомпилируется, поскольку конструктор Monstupar приватный,
-- поэтому конструировать парсеры тут можно только используя примитивные
-- парсеры из Core.

--------------------------------------------------------------------------------
-- Всякие удобные и полезные штуки

-- Всё плохо
notok :: Monstupar s ()
notok = isnot ok

-- В голове ввода сейчас в точности s
char :: (Eq s, Show s) => s -> Monstupar s s
char = like . (==)

-- В голове ввода сейчас что-то из списка
oneOf :: (Eq s, Show s) => [s] -> Monstupar s s
oneOf = like . flip elem

-- В префиксе головы сейчас нечто вполне определённое
string :: (Eq s, Show s) => [s] -> Monstupar s [s]
string (c:cs) = do
    c' <- char c
    cs' <- string cs
    return $ c' : cs'
string [] = return []

-- "Звёздочка" -- запустить парсер максимальное число раз и саккумулировать
-- результыты
many :: Monstupar s a -> Monstupar s [a]
many p = (do
        a <- p
        as <- many p
        return (a:as)) <|> return []
-- Аккуратно с реализацией! Следите за тем, чтобы у вас из-за использования <|>
-- не рос в бесконечность стек.

-- "Плюсик"
many1 :: Monstupar s a -> Monstupar s [a]
many1 p = do
    e <- p
    es <- many p
    return (e:es)

-- "Вопросик" -- ноль или один раз
optional :: Monstupar s a -> Monstupar s (Maybe a)
optional parser = (do
    a <- parser
    return $ Just a) <|> return Nothing

whitespace = [' ', '\n', '\t']
whitespacePar = many $ oneOf whitespace
