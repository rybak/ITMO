import System.Environment

import L.Abs
import L.Par (pParProgram)
import L.Lex (tokens, Token)
import L.Print
import qualified L.ErrM as ErrM

parseProg = pParProgram . tokens

main = do
	args <- getArgs
	input <- readFile $ head args
	case parseProg input of
		ErrM.Ok prog -> let
			ppProg = printTree prog
			in mapM_ putStrLn [ppProg, show prog]
		ErrM.Bad s -> putStrLn $ "Error : " ++ s
