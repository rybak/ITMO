-- Extremely simple but monstrously stupid (and slow) monadic parser
-- combinator library
module Monstupar.Core
    ( ParseError(..)
    , Monstupar, runParser
    , ok, isnot, eof, (<|>), like
    ) where

--------------------------------------------------------------------------------
-- Определения

-- Тело этого определения можно заменить на всё, что захочется
data ParseError = ParseError | ParseErrorMsg String
                deriving (Show) -- лишь бы show был

newtype Monstupar s a = Monstupar { runParser :: [s] -> Either ParseError ([s], a) }
-- runParser :: (Monstupar s a) -> [s] -> Either ParseError ([s], a)

instance Monad (Monstupar s) where
    return a = Monstupar $ \s -> Right (s , a)
-- (>>=) ::
-- (Monstupar s) a -> (a -> (Monstupar s) b) -> (Monstupar s) b
    ma >>= f = Monstupar $ \s -> case (runParser ma s) of
        Right (s', a) -> runParser (f a) s'
        Left e -> Left e
--    (>>) :: Monad m => m a -> m b -> m b
--    ma >> mb = ma >>= \_ -> mb
--------------------------------------------------------------------------------
-- Примитивные парсеры.
-- Имена и сигнатуры функций менять нельзя, тела можно

-- Всё хорошо
ok :: Monstupar s ()
ok = Monstupar $ \s -> Right (s , ())

-- Не должно парситься парсером p
isnot :: Monstupar s () -> Monstupar s ()
isnot p = Monstupar $ \s -> case runParser p s of
    Left e -> Right (s , ())
    Right _ -> Left $ ParseErrorMsg $ "parser isnot is not happy"

-- Конец ввода
eof :: Show s => Monstupar s ()
eof = Monstupar $ \s -> case s of
    [] -> Right (s , ())
    _  -> Left $ ParseErrorMsg $ "garbage: " ++ (show s)

infixr 2 <|>
-- Сначала первый парсер, если он фейлится, то второй
(<|>) :: Monstupar s a -> Monstupar s a -> Monstupar s a
a <|> b = Monstupar $ \s -> case runParser a s of
    Left _  -> runParser b s
    a -> a

-- В голове ввода сейчас нечто, удовлетворяющее p
like :: Show s => (s -> Bool) -> Monstupar s s
like p = Monstupar $ \s -> let a = head s in
    if null s
        then Left $ ParseErrorMsg $ "parser like does not like empty strings"
        else if p a
            then Right (tail s, a)
            else Left $ ParseErrorMsg $
                "parser like does not like " ++ (show s) ++ ", especially " ++ (show a)

-- Сюда можно добавлять ещё какие-то примитивные парсеры
-- если они понадобятся

