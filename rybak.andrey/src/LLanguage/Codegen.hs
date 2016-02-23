{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE FlexibleContexts #-}
module LLanguage.Codegen where

import L.Abs
import L.ErrM

import LLanguage.Annotated
import LLanguage.Utils

import LLVM.General.AST
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Global as G
import LLVM.General.AST.Instruction
import LLVM.General.AST.Operand
import qualified LLVM.General.Module as CModule

import Data.Monoid
import Control.Monad.State hiding (void)
import Control.Applicative
import qualified Data.Map as M

-- data Binding = Global String
--     deriving (Eq, Ord, Show, Read)

isVarArgSupported = False

newtype LLVM a = LLVM { unLLVM :: State Module a }
runLLVM :: Module -> LLVM a -> Module
runLLVM = flip (execState . unLLVM)

data BlockState = BlockState {
    indwx :: Integer,
    stack :: [Named Instruction],
    term :: Maybe (Named Terminator)
} deriving Show

type CGSymTab = [(String, Operand)]
data CodegenState = CodegenState {
    currentBlock :: Name,
    blocks :: M.Map Name BlockState,
    symtab :: CGSymTab,
    blockCount :: Integer,
    count :: Integer,
    names :: Names
}
emptyState :: CodegenState
emptyState = CodegenState (Name "vitaliy") M.empty [] 0 0 M.empty

newtype Codegen a = Codegen {
    runCodegen :: State CodegenState a
} deriving (Functor, Applicative, Monad, MonadState CodegenState)


codegen :: AProgram (Maybe ParLType) -> Module
codegen p = evalState (runCodegen $ compileProgram p) emptyState

compileProgram :: AProgram (Maybe ParLType) -> Codegen Module
compileProgram (AProg topLevels) = do
    let varDefs = map (GlobalDefinition . compileGlobalVar) $ getVars topLevels
    let funs = getFuns topLevels
    funsCode <- mapM codegenTopFun funs
    let funDefs = map GlobalDefinition funsCode
    return $ defaultModule {
            moduleName = "Main",
            moduleDefinitions = varDefs ++ funDefs
        }
getVars ts = [(pi, t) | ATopDecl (ADec pi t) <- ts]
getFuns ts = filter isATopFun ts

convertToLLVMType :: ParLType -> T.Type -- from LLVM.G.A.T
convertToLLVMType TInt = T.i32
convertToLLVMType TVoid = T.void

compileGlobalVar :: (PIdent, ParLType) -> G.Global
compileGlobalVar (pi, t) = G.globalVariableDefaults {
    G.name = compileName pi,
    G.type' = convertToLLVMType t
}

codegenTopFun :: ATopLevel a -> Codegen G.Global
codegenTopFun (ATopFun pi as rt b) = do
    bblocks <- codegenFunctionBody rt b
    return $ G.functionDefaults {
            G.returnType = compileType rt,
            G.name = compileName pi,
            G.parameters = (compileParameters as, isVarArgSupported),
            G.basicBlocks = bblocks
        }
codegenTopFun _ = error "Internal error: codegen function out of something else."

compileName pi = Name $ pIdentToString pi
compileType TVoid = T.void
compileType TInt = T.i32
compileType (TFun ats rt) = T.ptr $
    T.FunctionType (compileType rt) (map compileType ats) isVarArgSupported
compileParameters :: [ADecl] -> [G.Parameter]
compileParameters = map compileParameter -- TODO add "([Parameter], Bool)
compileParameter (ADec pi t) = G.Parameter (compileType t) (compileName pi) []

codegenFunctionBody :: ParLType -> ABlock a -> Codegen [G.BasicBlock]
codegenFunctionBody rt (ABlockB astms) = do
    nis <- mapM codegenStm astms
    let returnName = my_name "fun_ret_" "val"
    let returnNI = [my_alloca returnName rt]
    let entry = G.BasicBlock (Name "entry") ((concat nis) ++ returnNI) (Do $ Ret (Just $ local rt returnName) [])
    return $ [entry] -- : concat bblockss -- still a stub, TODO proper BasicBlock codegen

codegenStm :: AStm a -> Codegen ([Named Instruction])
codegenStm (ASDecl (ADec pi t)) =
    return $ [ my_alloca (my_name "var_" (pIdentToString pi)) t]
    --  ^^ Instruction data constructor
codegenStm _ = return []

my_name p v = Name $ p ++ v
my_alloca n t = n := (Alloca (compileType t) Nothing 0 [])
-- LLVM vars infrastructure
type Names = M.Map String Int
startIndex :: Int
startIndex = 0
uniqueName :: String -> Names -> (String, Names)
uniqueName name names = maybe
    (name ++ show startIndex, M.insert name startIndex names)
    (\index -> let newIndex = index + 1 in
        (name ++ show newIndex, M.insert name newIndex names))
    (M.lookup name names)

local :: ParLType -> Name -> Operand
local t = LocalReference (compileType t)
    
-- /LLVM vars
-- LLVM BasicBlock infrastructure

-- /LLVM BasicBlock

--"@" ++ name ++ " = global " ++ show llType ++ " 0"
{- // -}
