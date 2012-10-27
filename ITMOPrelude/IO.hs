{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Categories
import Prelude (Show,show)

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat } deriving (Show)

type IO a = State RealWorld a

getNat :: IO Nat
getNat = State $
    \rw -> (RealWorld (tail $ stdIn rw) (stdOut rw) (exitCode rw),
            head $ stdIn rw)

putNat :: Nat -> IO ()
putNat n = State $
    \rw -> (RealWorld (stdIn rw) (Cons n $ stdOut rw) (exitCode rw)
        , ())

setExitCode :: Nat -> IO ()
setExitCode code = State $
    \rw -> (RealWorld (stdIn rw) (stdOut rw) code, ())
