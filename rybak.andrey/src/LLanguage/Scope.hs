module LLanguage.Scope (
    checkScope,
	BuildSt (scope, symTab, errs),
	SymTab
) where

import L.Abs
import L.Print -- for pretty printing error messages
import L.ErrM
import LLanguage.Utils

import Control.Monad hiding (forM_)
import Data.Foldable (forM_)
import Utils.SM
import Data.Maybe
import qualified Data.Map as M

type Name = String
data SymTabItem = STVar PIdent ParLType
                  | STFun PIdent ParLType
                deriving (Eq,Show)


type Scope = (String, [Integer])
type ScopeListing = M.Map Name SymTabItem
type SymTab = M.Map Scope ScopeListing

data BuildSt = St {
        symTab :: SymTab,
        scope :: Scope,
        counter :: Integer,
        errs :: [String]
    } deriving Show

type Result = SM BuildSt ()

-- scope check
globalScopeStart :: Scope
globalScopeStart = ("", [])
predefinedFuncs = M.empty
checkScope :: ParProgram -> BuildSt
checkScope prog = let
    ((), buildStGlobal) = runState (collectGlobals prog) (St predefinedFuncs globalScopeStart 0 [])
    in
        execState (buildSTProgram prog) buildStGlobal

execState f s = snd $ runState f s
buildSTProgram prog = SM (\st -> ((), st)) -- ~same as undefined

{- collectGlobals before doing anything else
 - this is needed to simplify mutual recursion processing -}
collectGlobals :: ParProgram -> Result
collectGlobals (Prog topLevels) = collectGlobalVars topLevels
collectGlobalVars :: [ParTopLevel] -> Result
collectGlobalVars = mapM_ collectGlobalVar

collectGlobalVar :: ParTopLevel -> Result
collectGlobalVar x = case x of
	TopDecl (Dec pi parType) -> do
		let newVar = STVar pi parType
		addError <- addSymbolCurrentScope newVar
		forM_ addError $ duplicateError "global var: " newVar
		return ()
	_ -> return () -- do nothing with TopFun

showPI :: PIdent -> String
showPI (PIdent ((x,y), name)) = name ++ " at line " ++ show x ++ " column " ++ show y
showSTItem :: SymTabItem -> String
showSTItem (STVar pi parLType) = showPI pi ++ " of type " ++ printTree parLType -- maybe use prettyPrint
showSTItem _ = undefined

duplicateError :: String -> SymTabItem -> SymTabItem -> Result
duplicateError msg dup orig = addToErrs ("Name clash " ++ msg ++ showSTItem dup
	++ ", previously defined : " ++ showSTItem orig)

{- Nothing indicates no error, Just x indicates that symbol duplicates name of x -}
type Duplicate = SymTabItem
--addSymbolCurrentScope :: SymTabItem -> SM BuildSt (Maybe Duplicate)
addSymbolCurrentScope symbol = do
	scope <- getScope
	symbolTable <- getSymTab
	case M.lookup scope symbolTable of
		Nothing -> -- no scope at this level defined, start with empty listing
			insertNewSymbol symbol M.empty scope symbolTable
		Just scopeListing -> maybe (insertNewSymbol symbol scopeListing scope symbolTable) return (M.lookup (symTabItemToName symbol) scopeListing)

insertNewSymbol symbol scopeListing scope symTab = do
	let newScopeListing = M.insert (symTabItemToName symbol) symbol scopeListing
	let newSymbols = M.insert scope newScopeListing symTab
	setSymTab newSymbols
	return Nothing -- no error

getScope :: SM BuildSt Scope
getScope = SM (\st -> (scope st, st))
getSymTab :: SM BuildSt SymTab
getSymTab = SM (\st -> (symTab st, st))
setSymTab :: SymTab -> Result
setSymTab newSymTab = SM (\st -> ((), st { symTab = newSymTab }))

-- helper functions

symTabItemToName :: SymTabItem -> Name
symTabItemToName (STVar pi _) = pIdentToString pi

-- SM BuildSt helper functions
getErrs :: SM BuildSt [String]
getErrs = SM (\st -> (errs st,st))

setErrs :: [String] -> Result
setErrs newErrs = SM (\st -> ((), st { errs = newErrs }))

addToErrs :: String -> Result
addToErrs str = do
  errors <- getErrs
  setErrs (str:errors)

