import System.Environment

-- bnfc
import L.Abs
import L.Par (pParProgram)
import L.Lex (tokens, Token)
import L.Print
import qualified L.ErrM as ErrM
-- LLanguage
import LLanguage.Scope
import LLanguage.TypeCheck
import LLanguage.Codegen
import LLanguage.BuiltIn

parseProg = pParProgram . tokens

printLLVM :: ParProgram -> String
printLLVM prog = unlines [
		builtInConsts,
		builtInFunctions
		]


main = do
	args <- getArgs
	input <- readFile $ head args
	case parseProg input of
		ErrM.Ok prog -> do
			let ppProg = printTree prog
			mapM_ putStrLn [ppProg, show prog]
			let (aTree, buildst) = checkTypes prog
			case null (errs buildst) of
				True -> do
					putStrLn "Scope and type checks successfull."
					putStrLn $ show (scope buildst)
					putStrLn $ show (symTab buildst)
					putStrLn $ show aTree
					putStrLn ""
					putStrLn $ unlines $ codegen aTree
				False -> do
					putStrLn "Scope and type check errors:"
					mapM_ ((putStr "\t" >>) . putStrLn) (errs buildst)
		ErrM.Bad s -> putStrLn $ "Parser error : " ++ s

