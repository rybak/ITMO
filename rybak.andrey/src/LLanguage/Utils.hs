module LLanguage.Utils where

import L.Abs

pIdentToString :: PIdent -> String
pIdentToString (PIdent ((_,_), str)) = str

funDeclToFunType :: [ParLType] -> ParLType -> ParLType
funDeclToFunType = TFun
