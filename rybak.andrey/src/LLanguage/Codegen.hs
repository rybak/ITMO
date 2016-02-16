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
    let defs = map (LLVM.GlobalDefinition . compileGlobalVar) $ getVars topLevels
    return $ LLVM.defaultModule { LLVM.moduleName = "Main", LLVM.moduleDefinitions = defs }

getVars ts = [(pIdentToString pi, t) | ATopDecl (ADec pi t) <- ts]
convertToLLVMType :: ParLType -> T.Type -- from LLVM.G.A.T
convertToLLVMType TInt = T.i32
convertToLLVMType TVoid = T.void

compileGlobalVar :: (String, ParLType) -> G.Global
compileGlobalVar (varName, parType) = G.globalVariableDefaults {
    G.name = LLVM.Name varName,
    G.type' = convertToLLVMType parType
}

--"@" ++ name ++ " = global " ++ show llType ++ " 0"
{- // -}
