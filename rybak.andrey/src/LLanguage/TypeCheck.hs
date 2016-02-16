module LLanguage.TypeCheck where

import L.Abs
import L.Print -- for pretty printing error messages
import L.ErrM
import Utils.SM
import LLanguage.Annotated
import LLanguage.Scope
import LLanguage.Symtab
import LLanguage.Utils
import Control.Monad (when)

checkTypes :: ParProgram -> (AProgram (Maybe ParLType), BuildSt)
checkTypes prog =
    let
        buildst = checkScope prog
    in
        runState (typeProgram prog) buildst

-- use Annotated ASTs as a
type TypeCheckResult a = SM BuildSt a

typeProgram :: ParProgram -> TypeCheckResult (AProgram (Maybe ParLType))
typeProgram (Prog topLevels) = do
    tls <- mapM typeTopLevel topLevels
    return $ AProg tls

typeTopLevel :: ParTopLevel -> TypeCheckResult (ATopLevel (Maybe ParLType))
typeTopLevel (TopDecl decl) = do
    dec <- typeDecl decl
    return $ ATopDecl dec
typeTopLevel (TopFun pi args retType body) = do
    setScope (pIdentToString pi, [])
    ablock <- typeBlock body
    -- TODO empty typecheck
    resetScope
    return $ ATopFun pi (topFunToType args retType) ablock -- TODO ATopFun should not be empty

typeDecl :: Decl -> TypeCheckResult ADecl
typeDecl (Dec pi parType) = case parType of
  TVoid -> do
    addToErrs ("Can't declare Void variables : " ++ showPIwithType pi parType)
    return $ ADec pi parType
  _ -> return $ ADec pi parType

typeBlock :: Block -> TypeCheckResult (ABlock (Maybe ParLType))
typeBlock (BlockB stms) = do
    counter <- getCounter
    scope <- getScope
    pushScope
    setCounter 0
    astms <- mapM typeStm stms
    setScope scope
    setCounter (counter + 1)
    return $ ABlockB astms

typeStm :: ParStm -> TypeCheckResult (AStm (Maybe ParLType))
typeStm (SDecl decl) = do
    adecl <- typeDecl decl
    return $ ASDecl adecl
typeStm (Assign pi exp) = do
    aexp <- typeExp exp
    varInfo <- lookupSymCurScope (pIdentToString pi)
    case (getAExp aexp, varInfo) of
        (Just expType, Just (STVar _ varType)) -> do
            when (expType /= varType) $ addToErrs $ "Assignment type mismatch: '" ++ showPI pi ++ "'\n" ++ typeMismatch varType expType
            return $ AAssign pi aexp
        _ -> do
            addToErrs $ "Look for scope errors about " ++ showPI pi
            return $ AAssign pi aexp

getAExp :: AExp x -> x
getAExp (AIntLit _ x) = x
getAExp (AEVar _ x) = x
getAExp (AEFun _ x) = x
typeMismatch :: ParLType -> ParLType -> String
typeMismatch expectedType actualType = "\t\tExpected type:\t" ++ printTree expectedType ++ "\n\t\t  Actual type:\t" ++ printTree actualType

typeExp :: ParExp -> TypeCheckResult (AExp (Maybe ParLType))
typeExp (IntLit n) = return $ AIntLit n (Just TInt)
typeExp (EVar pi) = do
    varInfo <- lookupSymCurScope (pIdentToString pi)
    case varInfo of
        Just (STVar name parType) -> return $ AEVar pi (Just parType)
        Just (STFun name parType) -> return $ AEFun pi (Just parType)
        _ -> do
            addToErrs ("Internal ? error : there should be an error in scopecheck about : " ++ showPI pi)
            return $ AEVar pi Nothing


