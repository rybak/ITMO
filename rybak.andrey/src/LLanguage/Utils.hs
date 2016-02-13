module LLanguage.Utils where

import L.Abs

pIdentToString :: PIdent -> String
pIdentToString (PIdent ((_,_), str)) = str

funDeclToFunType :: [ParLType] -> ParLType -> ParLType
funDeclToFunType = TFun

declToType :: Decl -> ParLType
declToType (Dec _ t) = t

showPI :: PIdent -> String
showPI (PIdent ((x,y), name)) = name ++ " at line " ++ show x ++ " column " ++ show y
showSTItem :: SymTabItem -> String
showSTItem (STVar pi parLType) = showPIwithType pi parLType
showSTItem (STFun pi parLType) = showPIwithType pi parLType
showPIwithType pi t = showPI pi ++ " of type " ++ printTree t
