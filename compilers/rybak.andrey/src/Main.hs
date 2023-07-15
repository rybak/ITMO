import System.Environment

-- bnfc
import L.Abs
import L.Par (pParProgram)
import L.Lex (tokens)
import L.Print
import qualified L.ErrM as ErrM
-- LLanguage
import LLanguage.Scope
import LLanguage.TypeCheck
import LLanguage.Codegen
import LLanguage.BuiltIn

import qualified LLVM.General.Module as CLLVM
import qualified LLVM.General.AST as AST
import LLVM.General.Context

--import Control.Monad ((>=>))
import Control.Monad.Except
import qualified Data.Map as M

parseProg :: String -> ErrM.Err ParProgram
-- helper for binding with BNFC generated lexer and parser
parseProg = pParProgram . tokens

printLLVM :: ParProgram -> String
printLLVM prog = unlines [
        builtInConsts,
        builtInFunctions
        ]

printIR :: AST.Module -> IO ()
printIR mod = do
    withContext $ \context -> do
        x <- runExceptT $ CLLVM.withModuleFromAST context mod $ \m -> do
            llstr <- CLLVM.moduleLLVMAssembly m
            putStrLn llstr
        case x of
            Left e -> putStrLn $ "LLVM error: " ++ show e
            Right _ -> putStrLn "LLVM: Ok"

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  putStrLn $ "Parsed " ++ (show $ length $ lines input) ++ " lines."
  putStrLn $ printNumberedLines input
  case parseProg input of
    ErrM.Ok prog -> do
      let ppProg = printTree prog
      mapM_ putStrLn [ppProg, show prog]
      let (aTree, buildst) = checkTypes prog
      if null (errs buildst)
        then do
          putStrLn "=================================="
          putStrLn "Scope and type checks successfull."
          print $ scope buildst
          print $ symTab buildst
          print aTree
          putStrLn ""
          let myAST = codegen aTree
          printIR myAST
        else do
          putStrLn "============================"
          putStrLn "Scope and type check errors:"
          mapM_ ((putStr "\t" >>) . putStrLn) (reverse $ errs buildst)
          putStrLn $ unlines $ map show $ M.toList $ symTab buildst -- TODO remove debug output
    ErrM.Bad s -> putStrLn $ "Parser error : " ++ s

showLine :: Integer -> String -> String
showLine n s = show n ++ ":\t|" ++ s
printNumberedLines :: String -> String
printNumberedLines = unlines . (map (uncurry showLine)) . (zip [1..]) . lines
