import System.Environment

-- bnfc
import L.Abs
import L.Par (pParProgram)
import L.Lex (tokens, Token)
import L.Print
import qualified L.ErrM as ErrM
-- LLanguage
import LLanguage.BuiltIn
import LLanguage.Scope
import LLanguage.TypeCheck

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
		typeChecked = typeCheck prog


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
				False -> do
					putStrLn "Scope and type check errors:"
					mapM_ ((putStr "\t" >>) . putStrLn) (errs buildst)
		ErrM.Bad s -> putStrLn $ "Parser error : " ++ s

