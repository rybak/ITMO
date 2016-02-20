module LLanguage.Utils where

import L.Abs
import L.Print
import LLanguage.Symtab
import LLanguage.Annotated

pIdentToString :: PIdent -> String
pIdentToString (PIdent ((_,_), str)) = str

funParTypeToFunType :: [ParLType] -> ParLType -> ParLType
funParTypeToFunType = TFun

declToType :: Decl -> ParLType
declToType (Dec _ t) = t

topFunToType :: [Decl] -> ParLType -> ParLType
topFunToType decls = funParTypeToFunType (map declToType decls)

showPI :: PIdent -> String
showPI (PIdent ((x,y), name)) = name ++ " at line " ++ show x ++ " column " ++ show y
showSTItem :: SymTabItem -> String
showSTItem (STVar pi parLType) = showPIwithType pi parLType
showSTItem (STFun pi parLType) = showPIwithType pi parLType
showPIwithType pi t = showPI pi ++ " of type " ++ printTree t

isTopFun :: ParTopLevel -> Bool
isTopFun (TopFun _ _ _ _) = True
isTopFun _ = False
isATopFun (ATopFun _ _ _ _) = True
isATopFun _ = False
