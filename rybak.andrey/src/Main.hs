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

parseProg :: String -> ErrM.Err ParProgram
parseProg = pParProgram . tokens

printLLVM :: ParProgram -> String
printLLVM prog = unlines [
        builtInConsts,
        builtInFunctions
        ]

showLine :: Integer -> String -> String
showLine n s = show n ++ ":\t|" ++ s
printNumberedLines :: String -> String
printNumberedLines = unlines . (map (uncurry showLine)) . (zip [1..]) . lines

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
          putStrLn "Scope and type checks successfull."
          print $ scope buildst
          print $ symTab buildst
          print aTree
          putStrLn ""
          putStrLn $ unlines $ codegen aTree
        else do
          putStrLn "Scope and type check errors:"
          mapM_ ((putStr "\t" >>) . putStrLn) (errs buildst)
    ErrM.Bad s -> putStrLn $ "Parser error : " ++ s

