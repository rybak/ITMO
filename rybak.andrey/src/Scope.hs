module Scope (
    scopeCheck,
	BuildSt (scope, symTab, errs),
	SymTab
) where

import L.Abs
import L.Print -- for pretty printing error messages
import L.ErrM

import Control.Monad
import Utils.SM
import Data.Maybe
import qualified Data.Map as M

type Name = String
data SymTabItem = STVar PIdent ParLType deriving (Eq,Show)

type Scope = (String, [Integer])
type SymTab = M.Map Scope (M.Map Name SymTabItem)

data BuildSt = St {
        symTab :: SymTab,
        scope :: Scope,
        counter :: Integer,
        errs :: [String]
    } deriving Show

type Result = SM BuildSt ()

-- helper functions
pIdentToString :: PIdent -> String
pIdentToString (PIdent ((_,_), str)) = str

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

-- helper for building error messages:

-- scopeCheck
predefinedFuncs = M.empty
scopeCheck :: ParProgram -> BuildSt
scopeCheck prog = let
    ((), buildStGlobal) = runState (collectGlobals prog) (St predefinedFuncs ("", []) 0 [])
    in
        snd $ runState (buildSTProgram prog) buildStGlobal

buildSTProgram prog = SM (\st -> ((), st)) -- ~same as undefined

{- collectGlobals before doing anything else
 - this is needed to simplify mutual recursion processing -}
collectGlobals :: ParProgram -> Result
collectGlobals (Prog topLevels) = collectGlobalVars topLevels
collectGlobalVars :: [ParTopLevel] -> Result
collectGlobalVars [] = return ()
collectGlobalVars (tl : tls) = do
	collectGlobalVar tl
	collectGlobalVars tls

collectGlobalVar :: ParTopLevel -> Result
collectGlobalVar x = case x of
	TopDecl (Dec pi parType) -> do
		let newVar = STVar pi parType
		addError <- addSymbolCurrentScope newVar
		when (isJust addError) $
			duplicateError "global var: " newVar (fromJust addError)
		return ()
	_ -> undefined

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
addSymbolCurrentScope :: SymTabItem -> SM BuildSt (Maybe Duplicate)
addSymbolCurrentScope symbol = do
	scope <- getScope
	symTab <- getSymTab
	case M.lookup scope symTab of
		Nothing -> -- no scope at this level defined, start with empty SymTab
			insertNewSymbol symbol M.empty scope symTab
		Just scopeListing -> -- scope found, needs checking for duplicates
			case M.lookup (symTabItemToName symbol) scopeListing of
				Nothing -> -- no duplicate found, can insert
					insertNewSymbol symbol scopeListing scope symTab
				Just x -> return (Just x) -- return SymTabItem of duplicate

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


