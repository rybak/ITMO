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
globalScope :: Scope
globalScope = ("", [])
predefinedFuncs = M.empty -- add builtins later
checkScope :: ParProgram -> BuildSt
checkScope prog = let
    ((), buildStGlobal) = runState (collectGlobals prog) (St predefinedFuncs globalScope 0 [])
    in
        execState (buildSTProgram prog) buildStGlobal

execState f s = snd $ runState f s

{- collectGlobals before doing anything else
 - this is needed to simplify mutual recursion processing -}
collectGlobals :: ParProgram -> Result
collectGlobals (Prog topLevels) = mapM_ collectTopLevel topLevels

collectTopLevel :: ParTopLevel -> Result
collectTopLevel (TopDecl x) = newVariable STVar "global var" x
collectTopLevel (TopFun pi argsDecls retType body) = do
        let newFun = STFun pi (topFunToType argsDecls retType)
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
buildSTProgram (Prog topLevels) = mapM_ buildSTFunction $ filter isTopFun topLevels

buildSTFunction :: ParTopLevel -> Result
buildSTFunction (TopDecl _) = error "should not happen"
buildSTFunction (TopFun pi args retType body) = do
    setScope (pIdentToString pi, [])
    mapM_ buildSTArg args
    -- TODO add check for dead code after return
    -- TODO add check for returns in all execution paths
    buildSTBlock body
    resetScope

buildSTArg :: Decl -> Result
buildSTArg = newVariable STArg "function argument"

buildSTDecl :: Decl -> Result
buildSTDecl = newVariable STVar "local var"

buildSTBlock :: Block -> Result
buildSTBlock (BlockB statements) = do
    counter <- getCounter
    pushScope
    mapM_ buildSTStm statements
    popScope
    setCounter (counter + 1)

buildSTStm :: ParStm -> Result
buildSTStm (SDecl x) = buildSTDecl x
buildSTStm (Assign pi exp) = do
    inScope <- lookupSymCurScope (pIdentToString pi) 
    (case inScope of
        Nothing -> noDeclarationError pi
        (Just f@(STFun _ _)) -> addToErrs $ "Trying to assign to global function " ++ showPI pi ++ ", previously declared : " ++ showSTItem f
        (Just (STVar _ _)) -> return () -- assigning to a var, OK
        (Just arg@(STArg _ _)) -> addToErrs $ "Trying to assign to function argument " ++ showPI pi ++ ", declared as : " ++ showSTItem arg
        ) >> buildSTExp exp
buildSTStm (SRet exp) = buildSTExp exp
    
-- TODO : other statements
noDeclarationError :: PIdent -> Result
noDeclarationError pi = addToErrs $ "Using variable before declaration : " ++ showPI pi

buildSTExp :: ParExp -> Result
buildSTExp (EVar pi) = do
    inScope <- lookupSymCurScope (pIdentToString pi)
    when (isNothing inScope) $ noDeclarationError pi
buildSTExp (IntLit n) = return () -- nothing to do with integer literal

lookupSymCurScope :: Name -> SM BuildSt (Maybe SymTabItem)
lookupSymCurScope name = do
    scope <- getScope
    symbolTable <- getSymTab
    return $ upClose name scope symbolTable

upClose :: Name -> Scope -> SymTab -> Maybe SymTabItem
upClose name s@(n, []) tab = case M.lookup s tab of
    Nothing -> globalScopeLookup name tab
    Just sl -> case M.lookup name sl of
        Nothing -> globalScopeLookup name tab
        Just x -> Just x
upClose name s@(n,(x:xs)) tab = case M.lookup s tab of
    Nothing -> upClose name (n,xs) tab
    Just sl -> case M.lookup name sl of
        Nothing -> upClose name (n,xs) tab
        Just x -> Just x

globalScopeLookup :: Name -> SymTab -> Maybe SymTabItem
globalScopeLookup name tab = case M.lookup globalScope tab of
    Nothing -> error "no global scope"
    Just gsl -> case M.lookup name gsl of
        Nothing -> Nothing
        Just x -> Just x
-- helper functions

newVariable :: (PIdent -> ParLType -> SymTabItem)
    -> String -> Decl -> Result -- used for arguments, local and global vars
newVariable c kindOfVar (Dec pi parType) = do
    let newVar = c pi parType
    addError <- addSymbolCurrentScope newVar
    forM_ addError $ duplicateError kindOfVar newVar
    return ()

symTabItemToName :: SymTabItem -> Name
symTabItemToName (STVar pi _) = pIdentToString pi
symTabItemToName (STFun pi _) = pIdentToString pi
symTabItemToName (STArg pi _) = pIdentToString pi

-- SM BuildSt helper functions
getScope :: SM BuildSt Scope
{- TODO later : when processing calls to function need to 
    do
        scope <- getScope
        setScope globalScope
        --lookup here
        setScope scope`
-}
getScope = SM (\st -> (scope st, st))
setScope :: Scope -> Result
setScope newScope = SM (\st -> ((), st { scope = newScope }))
resetScope :: Result
resetScope = do
    setCounter 0
    setScope globalScope
pushScope :: Result
pushScope = do
    x <- getCounter
    (scopeTitle, xs) <- getScope
    setScope (scopeTitle, x:xs)
    setCounter 0
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

