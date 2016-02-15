module LLanguage.Annotated where

import L.Abs

data AProgram a = AProg [ATopLevel a]
    deriving (Eq, Ord, Show)
data ATopLevel a = ATopDecl ADecl | ATopFun PIdent ParLType (ABlock a)
    deriving (Eq, Ord, Show)

data ADecl = ADec PIdent ParLType
    deriving (Eq, Ord, Show)

data ABlock a = ABlockB [AStm a]
    deriving (Eq, Ord, Show)
data AStm a = ASDecl ADecl | AAssign PIdent (AExp a)
    deriving (Eq, Ord, Show)

data AExp a = AIntLit Integer a | AEVar PIdent a | AEFun PIdent a
    deriving (Eq, Ord, Show)
