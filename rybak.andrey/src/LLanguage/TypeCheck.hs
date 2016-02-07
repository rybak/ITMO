module LLanguage.TypeCheck where

import L.Abs
-- import L.Print -- for pretty printing error messages
import L.ErrM
import Utils.SM
import LLanguage.Annotated
import LLanguage.Scope


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
typeTopLevel _ = undefined

typeDecl :: Decl -> TypeCheckResult ADecl
typeDecl (Dec pi parType) = return $ ADec pi parType

