import System.Environment

-- bnfc
import L.Abs
import L.Par (pParProgram)
import L.Lex (tokens, Token)
import L.Print
import qualified L.ErrM as ErrM
-- LLanguage
import LLanguage.BuiltIn
-- Compiler
import Scope

parseProg = pParProgram . tokens

typeCheck = undefined
desugar = undefined
codegen = undefined

printLLVM :: ParProgram -> String
printLLVM prog = unlines [
		builtInConsts,
		codegen typeChecked,
		builtInFunctions
		]
	where
		typeChecked = typeCheck scopeChecked
		scopeChecked = scopeCheck prog


main = do
	args <- getArgs
	input <- readFile $ head args
	case parseProg input of
		ErrM.Ok prog -> do
			let ppProg = printTree prog
			mapM_ putStrLn [ppProg, show prog]
			let scopeCheckResult = scopeCheck prog
			case null (errs scopeCheckResult) of
				True -> do
					putStrLn "Scope check successfull."
					putStrLn $ show (scope scopeCheckResult)
					putStrLn $ show (symTab scopeCheckResult)
				False -> do
					putStrLn "Scope check errors:"
					mapM_ ((putStr "\t" >>) . putStrLn) (errs scopeCheckResult)
		ErrM.Bad s -> putStrLn $ "Error : " ++ s
