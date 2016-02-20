{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE FlexibleContexts #-}
module LLanguage.Codegen where

import L.Abs
import L.ErrM

import LLanguage.Annotated
import LLanguage.Utils

import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.Module as CModule

import Data.Monoid
import Control.Monad.State hiding (void)
import Control.Applicative
import qualified Data.Map as M

-- data Binding = Global String
--     deriving (Eq, Ord, Show, Read)

isVarArgSupported = False

data CodegenState = CodegenState {
    counter :: Int
}
emptyState = CodegenState 0

newtype Codegen a = Codegen {
    runCodegen :: State CodegenState a
} deriving (Functor, Applicative, Monad, MonadState CodegenState)

fresh :: String -> Codegen String
fresh pref = do
  x <- gets counter
  modify (\s -> s{counter = x + 1})
  return $ pref ++ show x

codegen :: AProgram (Maybe ParLType) -> LLVM.Module
codegen p = evalState (runCodegen $ compileProgram p) emptyState

compileProgram :: AProgram (Maybe ParLType) -> Codegen LLVM.Module
compileProgram (AProg topLevels) = do
    let varDefs = map (LLVM.GlobalDefinition . compileGlobalVar) $ getVars topLevels
    let funs = getFuns topLevels
    funsCode <- mapM codegenTopFun funs
    let funDefs = map LLVM.GlobalDefinition funsCode
    return $ LLVM.defaultModule {
            LLVM.moduleName = "Main",
            LLVM.moduleDefinitions = varDefs ++ funDefs
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
compileName pi = LLVM.Name $ pIdentToString pi

codegenTopFun :: ATopLevel a -> Codegen G.Global
codegenTopFun (ATopFun pi as rt b) = do
    bblocks <- codegenFunctionBody b
    return $ G.functionDefaults {
            G.returnType = compileType rt,
            G.name = compileName pi,
            G.parameters = (compileParameters as, isVarArgSupported),
            G.basicBlocks = bblocks
        }
codegenTopFun _ = error "Internal error: codegen function out of something else."

compileType TVoid = T.void
compileType TInt = T.i32
compileType (TFun ats rt) = T.ptr $
    T.FunctionType (compileType rt) (map compileType ats) isVarArgSupported
compileParameters :: [ADecl] -> [G.Parameter]
compileParameters = map compileParameter -- TODO add "([Parameter], Bool)
compileParameter (ADec pi t) = G.Parameter (compileType t) (compileName pi) []

codegenFunctionBody :: ABlock a -> Codegen [G.BasicBlock]
codegenFunctionBody _ = return [] -- stub TODO add codegen


--"@" ++ name ++ " = global " ++ show llType ++ " 0"
{- // -}
