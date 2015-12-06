import System.Environment

-- bnfc
import L.Abs
import L.Par (pParProgram)
import L.Lex (tokens, Token)
import L.Print
import qualified L.ErrM as ErrM
-- LLanguage
import LLanguage.BuiltIn

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
		scopeChecked = scopeCheck desugared
		desugared = desugar prog


main = do
	args <- getArgs
	input <- readFile $ head args
	case parseProg input of
		ErrM.Ok prog -> let
			ppProg = printTree prog
			in mapM_ putStrLn [ppProg, show prog]
		ErrM.Bad s -> putStrLn $ "Error : " ++ s
