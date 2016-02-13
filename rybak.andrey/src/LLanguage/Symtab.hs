module LLanguage.Symtab where

import L.Abs

import qualified Data.Map as M

type Name = String
data SymTabItem = STVar PIdent ParLType
                  | STFun PIdent ParLType
                deriving (Eq,Show)


type Scope = (String, [Integer])
type ScopeListing = M.Map Name SymTabItem
type SymTab = M.Map Scope ScopeListing
