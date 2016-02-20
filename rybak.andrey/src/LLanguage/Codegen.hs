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

data Binding = Global String
    deriving (Eq, Ord, Show, Read)

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

getVars ts = [(pIdentToString pi, t) | ATopDecl (ADec pi t) <- ts]
getFuns ts = filter isATopFun ts

convertToLLVMType :: ParLType -> T.Type -- from LLVM.G.A.T
convertToLLVMType TInt = T.i32
convertToLLVMType TVoid = T.void

compileGlobalVar :: (String, ParLType) -> G.Global
compileGlobalVar (varName, parType) = G.globalVariableDefaults {
    G.name = LLVM.Name varName,
    G.type' = convertToLLVMType parType
}

codegenTopFun :: ATopLevel a -> Codegen G.Global
codegenTopFun (ATopFun pi as rt b) = do
    ablocks <- codegenFunctionBody b
    return $ G.functionDefaults {
            G.returnType = compileType rt,
            G.name = LLVM.Name $ pIdentToString pi,
            G.parameters = compileParameters as,
            G.basicBlocks = ablocks
        }
codegenTopFun _ = error "Internal error : trying to codegen a function out of something else."

compileType = const T.i32 -- TODO add ints and function pointers and stuff
compileParameters = const ([], False) -- TODO add "([Parameter], Bool)
codegenFunctionBody _ = return [] -- stub TODO add codegen


--"@" ++ name ++ " = global " ++ show llType ++ " 0"
{- // -}
