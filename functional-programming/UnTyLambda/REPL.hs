-- REPL for untyped lambda calculus
module UnTyLambda.REPL where

import Monstupar
import Monstupar.Core
import Monstupar.Derived
import UnTyLambda.Interpreter
import Prelude hiding (catch)
import Control.Exception hiding (catch)

import System.Console.Haskeline

alphaPar = oneOf ['a'..'z']

-- Парсим строку в терм
parseLambda, parseVar, parseLam, parseLambda' :: Monstupar Char Term

parseLambda = do
    t <- parseLambda'
    eof
    return t

parseOneLambda = (do
        whitespacePar
        char '('
        t <- parseLambda'
        char ')'
        whitespacePar
        return $ t) <|> parseVar <|> parseLam

parseLambda' = do
        ts <- many1 (whitespacePar >> parseOneLambda)
        whitespacePar
        return $ foldl1 App ts

parseVar = do
    whitespacePar
    name <- parseVarName
    whitespacePar
    return $ Var name

parseVarName :: Monstupar Char Variable
parseVarName = do
    pref <- many $ char '_'
    name <- many1 $ alphaPar
    suff <- many $ char '\''
    return $ pref ++ name ++ suff

parseLam = do
    whitespacePar
    char lam
    whitespacePar
    var <- parseVarName
    whitespacePar
    char '.'
    whitespacePar
    term <- parseLambda'
    return $ Lam var term

--------------------------------------------------------------------------------
-- Заметьте, что грамматика лямбда-выражений леворекурсивна.
-- Перед тем как бросаться кодить, сначала уберите леворекурсивность
-- (неопределённость тоже стоит убрать) на бумаге, а потом напишите
-- получившуюся грамматику в EBNF вот сюда:
-- L = '(' L ')' A | V A | '(' L ')' | V | λ
-- V = {'_'} {'a'..'z'} {'\''}
-- λ = 'λ' V '.' L
-- A = L A | L
-- прямо сюда, да
--------------------------------------------------------------------------------

-- Красиво печатаем терм (можно с лишними скобками, можно без)
lam = '\0092'
prettyPrint :: Term -> String
prettyPrint (Var v) = v
prettyPrint (Lam v t) = 'λ' : v ++ "." ++ prettyPrint t
prettyPrint (App (Var v1) (Var v2)) = v1 ++ " " ++ v2
prettyPrint (App (Var v) t) = v ++ " (" ++ prettyPrint t ++ ")"
prettyPrint (App t (Var v)) = '(' : prettyPrint t ++ ") " ++ v
prettyPrint (App t1 t2) = "(" ++ prettyPrint t1 ++ ") (" ++
    prettyPrint t2 ++ ")"

-- Собственно сам REPL. Первый аргумент — максимальное число итераций при
-- попытке нормализации стратегией из второго аргумента.
replLoop :: Integer -> (Integer -> Term -> Term) -> IO ()
replLoop patience strategy = runInputT defaultSettings repl where
  repl = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> outputStrLn "Bye-bye."
      Just s -> do
        catch (outputStrLn $ case runParser parseLambda s of
            Left (ParseErrorMsg msg) -> "Error: " ++ msg
            Left _        -> "Error"
            Right ("", t) -> prettyPrint $ strategy patience t
            Right (s', t) -> "Error in parser : " ++ s'
          ) (\e -> outputStrLn $ show (e :: SomeException))
        repl
-- Диалог с (replLoop 100 no) должен выглядеть так:
-- > \x . (\y . y) x x
-- \x . x x

main = replLoop 100 wh



