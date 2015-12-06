module Scope (
    scopeCheck
) where

import L.Abs
import L.ErrM

--import Control.Monad
import Utils.SM
import Data.Maybe
import qualified Data.Map as M

type Name = String
data SymTabKind = STVar Name ParLType deriving (Eq,Show)

type Scope = (String, [Integer])
type SymTab = M.Map Scope (M.Map Name SymTabKind)

data BuildSt = St {
        syms :: SymTab,
        scope :: Scope,
        counter :: Integer,
        errs :: [String]
    } deriving Show

type Result = SM BuildSt ()

-- helper functions
pIdentToName :: PIdent -> Name
pIdentToName (PIdent ((_,_), str)) = str

symTabKindToName :: SymTabKind -> Name
symTabKindToName (STVar name _) = name

-- SM BuildSt helper functions
getErrs :: SM BuildSt [String]
getErrs = SM (\st -> (errs st,st))

setErrs :: [String] -> SM BuildSt ()
setErrs strs = SM (\st -> ((), St (syms st) (scope st) (counter st) strs))

addToErrs :: String -> SM BuildSt ()
addToErrs str = do
  errors <- getErrs
  setErrs (str:errors)

-- helper for building error messages:
getErrorStringCoords :: PIdent -> String
getErrorStringCoords (PIdent ((x,y),name)) = name ++ " at line " ++ show x ++ " col " ++ show y
-- errors
duplicateError :: PIdent -> SM BuildSt ()
duplicateError name = addToErrs ("Duplicate declared : " ++ getErrorStringCoords name)

-- scopeCheck
predefinedFuncs = M.empty
scopeCheck :: ParProgram -> BuildSt
scopeCheck prog = let
    ((), buildStGlobal) = runState (collectGlobal prog) (St predefinedFuncs ("", []) 0 [])
    in
        snd $ runState (buildSTProgram prog) buildStGlobal

buildSTProgram = undefined

collectGlobal = undefined


