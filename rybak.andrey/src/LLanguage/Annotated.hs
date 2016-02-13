module LLanguage.Annotated where

import L.Abs

data AProgram a = AProg [ATopLevel a]
    deriving (Eq, Ord, Show)
data ATopLevel a = ATopDecl ADecl | ATopFun String
    deriving (Eq, Ord, Show)

data ADecl = ADec PIdent ParLType
    deriving (Eq, Ord, Show)

data ABlock a = ABlockB [AStm a]
    deriving (Eq, Ord, Show)
data AStm a = ASDecl ADecl
    deriving (Eq, Ord, Show)
