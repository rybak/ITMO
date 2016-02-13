module LLanguage.Scope where

import L.Abs
import L.Print -- for pretty printing error messages
import L.ErrM
import LLanguage.Symtab
import LLanguage.Utils

import Control.Monad hiding (forM_)
import Data.Foldable (forM_)
import Utils.SM
import Data.Maybe
import qualified Data.Map as M

data BuildSt = St {
        symTab :: SymTab,
        scope :: Scope,
        counter :: Integer,
        errs :: [String]
    } deriving Show

type Result = SM BuildSt ()

-- scope check
emptyScope :: Scope
emptyScope = ("", [])
predefinedFuncs = M.empty -- add builtins later
checkScope :: ParProgram -> BuildSt
checkScope prog = let
    ((), buildStGlobal) = runState (collectGlobals prog) (St predefinedFuncs emptyScope 0 [])
    in
        execState (buildSTProgram prog) buildStGlobal

execState f s = snd $ runState f s

{- collectGlobals before doing anything else
 - this is needed to simplify mutual recursion processing -}
collectGlobals :: ParProgram -> Result
collectGlobals (Prog topLevels) = mapM_ collectTopLevel topLevels

collectTopLevel :: ParTopLevel -> Result
collectTopLevel (TopDecl x) = newVariable "global" x
collectTopLevel (TopFun pi argsDecls retType body) = do
        let newFun = STFun pi (funDeclToFunType (map declToType argsDecls) retType)
        addError <- addSymbolCurrentScope newFun
        forM_ addError $ duplicateError "global function: " newFun
        return ()

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
        Just scopeListing -> maybe -- maybe :: onNothing -> onJust -> Maybe
            (insertNewSymbol symbol scopeListing scope symbolTable) -- check didn't find anything, no error
            (return . Just) -- just return error
            (M.lookup (symTabItemToName symbol) scopeListing) -- check existing scopeListing

-- insert new symbol into scope listing without checks
insertNewSymbol symbol scopeListing scope symTab = do
    let newScopeListing = M.insert (symTabItemToName symbol) symbol scopeListing
    let newSymbols = M.insert scope newScopeListing symTab
    setSymTab newSymbols
    return Nothing -- no error by definition

-- /collectGlobals

buildSTProgram :: ParProgram -> Result
buildSTProgram (Prog topLevels) = mapM_ buildSTFunction $ filter isFunction topLevels
isFunction :: ParTopLevel -> Bool
isFunction (TopFun _ _ _ _) = True
isFunction _ = False

buildSTFunction :: ParTopLevel -> Result
buildSTFunction (TopDecl _) = error "should not happen"
buildSTFunction (TopFun pi args retType body) = do
    setScope (pIdentToString pi, [])
    mapM_ buildSTDecl args
    -- TODO add check for dead code after return
    -- TODO add check for returns in all execution paths
    buildSTBlock body
    resetScope

buildSTDecl :: Decl -> Result
buildSTDecl = newVariable "local"

buildSTBlock :: Block -> Result
buildSTBlock (BlockB statements) = do
    counter <- getCounter
    scope <- getScope
    pushScope
    setCounter 0
    mapM_ buildSTStm statements
    setCounter (counter + 1)

buildSTStm :: ParStm -> Result
buildSTStm (SDecl x) = buildSTDecl x
-- TODO : other statements

-- helper functions

newVariable :: String -> Decl -> Result -- used both for local and global vars
newVariable kindOfVar (Dec pi parType) = do
    let newVar = STVar pi parType
    addError <- addSymbolCurrentScope newVar
    forM_ addError $ duplicateError (kindOfVar ++ " var: ") newVar
    return ()

symTabItemToName :: SymTabItem -> Name
symTabItemToName (STVar pi _) = pIdentToString pi
symTabItemToName (STFun pi _) = pIdentToString pi

-- SM BuildSt helper functions
getScope :: SM BuildSt Scope
getScope = SM (\st -> (scope st, st))
setScope :: Scope -> Result
setScope newScope = SM (\st -> ((), st { scope = newScope }))
resetScope :: Result
resetScope = do
    setCounter 0
    setScope emptyScope
pushScope :: Result
pushScope = do
    x <- getCounter
    (scopeTitle, xs) <- getScope
    setScope (scopeTitle, x:xs)
popScope :: Result
popScope = do
    (s, (_ : xs)) <- getScope
    setScope (s, xs)

getSymTab :: SM BuildSt SymTab
getSymTab = SM (\st -> (symTab st, st))
setSymTab :: SymTab -> Result
setSymTab newSymTab = SM (\st -> ((), st { symTab = newSymTab }))

getCounter :: SM BuildSt Integer
getCounter = SM (\st -> (counter st, st))
setCounter :: Integer -> Result
setCounter newCounter = SM (\st -> ((), st { counter = newCounter }))

getErrs :: SM BuildSt [String]
getErrs = SM (\st -> (errs st,st))
setErrs :: [String] -> Result
setErrs newErrs = SM (\st -> ((), st { errs = newErrs }))

addToErrs :: String -> Result
addToErrs str = do
  errors <- getErrs
  setErrs (str:errors)

