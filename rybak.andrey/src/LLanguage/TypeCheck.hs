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
    adecls <- mapM typeDecl args
    resetScope
    return $ ATopFun pi adecls retType ablock

typeDecl :: Decl -> TypeCheckResult ADecl
typeDecl decl@(Dec pi parType) = case parType of
  TVoid -> do
    addToErrs ("Can't declare Void variables : " ++ showPIwithType pi parType)
    return $ ADec pi parType
  _ -> do
    return $ ADec pi parType

typeBlock :: Block -> TypeCheckResult (ABlock (Maybe ParLType))
typeBlock (BlockB stms) = do
    counter <- getCounter
    pushScope
    astms <- mapM typeStm stms
    popScope
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
typeStm (SRet exp) = do
    aexp <- typeExp exp
    typeReturn (printTree exp) (Just aexp)
typeStm SVoidRet = do
    typeReturn "nothing" Nothing

typeReturn :: String -> Maybe (AExp (Maybe ParLType)) -> TypeCheckResult (AStm (Maybe ParLType))
typeReturn returned me = do
    scope@(fn, _) <- getScope
    setScope globalScope
    funIn <- lookupSymCurScope fn
    setScope scope
    let mt = maybe (Just TVoid) getAExp me
    case (mt, funIn) of
        (Just et, Just f@(STFun _ ft)) -> do
            when (et /= returnType ft) $ addToErrs $ "Trying to return " ++ returned ++ " of type " ++ printTree et ++ " from function " ++ showSTItem f
            return $ ARet me
        (Nothing, _) -> internalError $ "Caught returning of something without type"

returnType :: ParLType -> ParLType
returnType (TFun _ rt) = rt
returnType x = internalError $ "Can not take a return type from " ++ printTree x
getAExp :: AExp (Maybe ParLType) -> (Maybe ParLType)
getAExp (AIntLit _) = (Just TInt)
getAExp (AEVar _ x) = x
getAExp (AEFun _ x) = x
typeMismatch :: ParLType -> ParLType -> String
typeMismatch expectedType actualType = "\t\tExpected type:\t" ++ printTree expectedType ++ "\n\t\t  Actual type:\t" ++ printTree actualType

typeExp :: ParExp -> TypeCheckResult (AExp (Maybe ParLType))
typeExp (IntLit n) = return $ AIntLit n
typeExp (EVar pi) = do
    varInfo <- lookupSymCurScope (pIdentToString pi)
    case varInfo of
        Just (STVar name parType) -> return $ AEVar pi (Just parType)
        Just (STFun name parType) -> return $ AEFun pi (Just parType)
        _ -> do
            addToErrs ("Internal ? error : there should be an error in scopecheck about : " ++ showPI pi)
            return $ AEVar pi Nothing


