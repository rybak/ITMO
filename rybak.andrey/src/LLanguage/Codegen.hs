{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE FlexibleContexts #-}
module LLanguage.Codegen where

import L.Abs
import L.ErrM

import LLanguage.Annotated
import LLanguage.Utils

import Data.Monoid
import Control.Monad.State
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

codegen :: AProgram (Maybe ParLType) -> [String]
codegen p = evalState (runCodegen $ compileProgram p) emptyState

compileProgram :: AProgram (Maybe ParLType) -> Codegen [String]
compileProgram (AProg topLevels) = do
    let gvars = getVars topLevels
        llVars = M.map convertToLLVMType gvars
        gVarsText = map compileGlobalVar (M.toList llVars)
    return $ gVarsText

getVars ts = M.fromList [(pIdentToString pi, t) | ATopDecl (ADec pi t) <- ts]
convertToLLVMType :: ParLType -> LLVMType
convertToLLVMType TInt = TInt32

data LLVMType = TInt32
instance Show LLVMType where
    show TInt32 = "i32"

compileGlobalVar (name, llType) = "@" ++ name
    ++ " = global " ++ show llType ++ " 0"
{- // -}
