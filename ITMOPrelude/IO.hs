{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Categories

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat }

type IO a = State RealWorld a

getNat :: IO Nat
getNat = State $
    \rw -> (RealWorld (tail $ stdIn rw) (stdOut rw) (exitCode rw),
            head $ stdIn rw)

-- putNat :: Nat -> IO ()
-- putNat = ?
-- 
-- setExitCode :: Nat -> IO ()
-- setExitCode = ?
