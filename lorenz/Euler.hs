{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeOperators, MultiParamTypeClasses, RankNTypes, UnboxedTuples, FunctionalDependencies, RecordWildCards, ExistentialQuantification, BangPatterns #-}


import Prelude ( Show(..)
               , error, undefined
               , Bool(..), (&&), (||), not
               , Int
               , String
               , Float, Double
               , fromIntegral, realToFrac
               , pi, sin, cos, asin, acos, atan, atan2, sqrt
               , Monad(..), (.), ($) )
import qualified Prelude as P
 
-- Rybak imports
import Text.Printf
import Data.List
import Control.Monad hiding(mzero)
import System.Environment(getArgs)
import Data.Sequence (index, iterateN)
import Fusion
import Graphics.EasyPlot
import System.Process
import Control.Concurrent.ParallelIO
-- Algebra
 
class Monoid a where
  mzero   :: a
  mappend :: a -> a -> a
  mconcat :: [ a ] -> a
  mconcat = P.foldr mappend mzero
 
-- Instances
 
instance Monoid Float where
  mzero = 0
  mappend = (P.+)
 
instance Monoid Double where
  mzero = 0
  mappend = (P.+)
 
--
 
class Monoid a => Group a where
  ginv :: a -> a
  gsub :: a -> a -> a
  gsub x y = mappend x (ginv y)
 
instance Group Float where
  ginv = P.negate
  gsub = (P.-)
 
instance Group Double where
  ginv = P.negate
  gsub = (P.-)
 
---
 
class Group a => Ring a where -- very Monoidy
  rone  :: a
  rmult :: a -> a -> a
 
instance Ring Float where
  rone = 1
  rmult = (P.*)
 
instance Ring Double where
  rone = 1
  rmult = (P.*)
 
--
 
class Ring a => Field a where
  recip  :: a -> a
 
  (/)    :: a -> a -> a
  (/) x y = rmult x (recip y)
 
(+) :: Monoid a => a -> a -> a
(+) = mappend
 
negate :: Group a => a -> a
negate = ginv
 
(-) :: Group a => a -> a -> a
(-) = gsub
 
(*) :: Ring a => a -> a -> a
(*) = rmult
 
instance Field Float where
  recip = P.recip
  (/) = (P./)
 
instance Field Double where
  recip = P.recip
  (/) = (P./)
 
-- Vector Spaces
 
class (Group v, Field s) => VectorSpace v s | v -> s where
  (^*) :: s -> v -> v
  (^/) :: v -> s -> v
  (^/) v x = (^*) (recip x) v
 
--
 
class BasicVector v where
  diagonal :: s -> v s
  vmap     :: (s -> s) -> v s -> v s
  vzip     :: (s -> s -> s) -> v s -> v s -> v s
  vfold    :: (s -> s -> s) -> v s -> s
 
instance (Monoid s, BasicVector v) => Monoid (v s) where
  mzero = diagonal mzero
  mappend = vzip mappend
 
instance (Group s, BasicVector v) => Group (v s) where
  ginv = vmap ginv
  gsub = vzip gsub
 
instance (Field s, BasicVector v) => VectorSpace (v s) s where
  (^*) s = vmap (s *)
  (^/) v s = vmap (/ s) v
 
-- 2D Vectors
 
data Vector2 a = Vector2 !a !a
                deriving Show
 
instance BasicVector Vector2 where
  diagonal s = Vector2 s s
  vmap f (Vector2 x y) = Vector2 (f x) (f y)
  vzip f (Vector2 x y) (Vector2 x' y') = Vector2 (f x x') (f y y')
  vfold o (Vector2 x y) = x `o` y
 
-- 3D Vectors
 
data Vector3 a = Vector3 !a !a !a
                deriving Show
 
instance BasicVector Vector3 where
  diagonal s = Vector3 s s s
  vmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)
  vzip f (Vector3 x y z) (Vector3 x' y' z') = Vector3 (f x x') (f y y') (f z z')
  vfold o (Vector3 x y z) = x `o` y `o` z
 
--
 
data Color4 a = Color4 !a !a !a !a
                deriving Show
 
instance BasicVector Color4 where
  diagonal s = Color4 s s s s
  vmap f (Color4 x y z a) = Color4 (f x) (f y) (f z) (f a)
  vzip f (Color4 x y z a) (Color4 x' y' z' a') = Color4 (f x x') (f y y') (f z z') (f a a')
  vfold o (Color4 x y z a) = x `o` y `o` z `o` a
 
-- instance Functor [] where
--   fmap = P.map
--  
-- instance Functor ((->) c) where
--   fmap f g c = f (g c)
 
--class Applicative f where
--  pure :: a -> f a
--  (<$>) :: f (a -> b) -> f a -> f b
--instance Functor ((←) c) where
--  fmap f g c = hole
 
--
 
infixr 4 +
infixr 4 -
infixr 5 ^*
infixr 5 ^/
infixr 5 *
infixr 5 /
 
-- Rybak's code
 
fx, fy, fz :: (Monoid a, Group a, Ring a) => a -> a -> a-> Vector3 a -> a
fx s _ _ (Vector3 x y _) =  s * (y - x)
fy _ r _ (Vector3 x y z) =  x * (r - z) - y
fz _ _ b (Vector3 x y z) =  x * y - b * z

-- toList (Vector3 a b c) = [a, b, c]

--takeWhile1 :: (a -> Bool) -> [a] -> [a]
--takeWhile1 _ [] = []
--takeWhile1 p (x:xs) | p x = x : takeWhile1 p xs
--                    | P.otherwise = [x]

notNaN :: Vector3 Float -> Bool
notNaN (Vector3 x y z) = P.not ((P.isNaN x) P.|| (P.isNaN y) P.|| (P.isNaN z))

explicitEuler, implicitEuler, rungeKutta4 :: Float -> Float -> Float -> Float -> Vector3 Float -> Vector3 Float
explicitEuler s r b dt v@(Vector3 x y z) = Vector3 (x + dt * dx) (y + dt * dy) (z + dt * dz) where
                (Vector3 dx dy dz) = applyf s r b v

ieK :: Int
ieK = 100
implicitEuler s r b h v = simpleIter (explicitEuler s r b h v)  where
    simpleIter vi = index (iterateN ieK (oneIter vi) vi) (ieK P.- 1)
    oneIter (Vector3 x y z) vs = Vector3 (x + h * dx) (y + h * dy) (z + h * dz) where
        (Vector3 dx dy dz) = applyf s r b vs

rungeKutta4 s r b h v = let
  f vv = applyf s r b vv
  k1 = h ^* f v
  k2 = h ^* f (v + 0.5 ^* k1)
  k3 = h ^* f (v + 0.5 ^* k2)
  k4 = h ^* f (v + k3)
  d = (1.0 / 6.0) ^* (k1 + 2 ^* k2 + 2 ^* k3 + k4)
  in v + d
  
myIterate :: (a -> a) -> a -> [a]
myIterate f = unstreamList . iterateStream f

lorenz :: (Float -> Float -> Float -> Float -> (Vector3 Float) -> Vector3 Float) -> Float -> Float -> Float -> Float -> (Vector3 Float) -> [Vector3 Float]
lorenz next s r b h start = takeWhile1 notNaN $ myIterate (next s r b h) start

type MethodType = Float -> Float -> Float -> Float -> Vector3 Float -> [Vector3 Float]
explicitEulerList, implicitEulerList, rungeKutta4List, adamsList :: MethodType
explicitEulerList = lorenz explicitEuler
implicitEulerList = lorenz implicitEuler
rungeKutta4List = lorenz rungeKutta4

adamsK :: Int
adamsK = 4

adamsOne2, adamsOne3, adamsOne4 :: Float -> [Vector3 Float] -> Vector3 Float -> Vector3 Float
adamsOne4 h [y0', y1', y2', y3'] y3 = y3 + h ^* (55.0 P./ 24.0 ^* y3' - 59.0 P./ 24.0 ^* y2' + 37.0 P./ 24.0 ^* y1' - 9.0 P./ 24.0 ^* y0')
adamsOne3 h [y0', y1', y2', y3'] y3 = y3 + h ^* (23.0 P./ 12.0 ^* y3' - 16.0 P./ 12.0 ^* y2' + 5.0 P./ 12.0 ^* y1') 
adamsOne2 h [y0', y1', y2', y3'] y3 = y3 + h ^* (1.5 ^* y3' - 0.5 ^* y2')
--adamsOne _ _ _ = error "adamsOne"

-- to R: calc first four derivativies (y') and pass y_4 too
adams :: Float -> Float -> Float -> Float -> [Vector3 Float] -> Vector3 Float -> [Vector3 Float]
adams s r b h dvs@[_ , v1', v2', v3'] v3 = let
    v4 = adamsOne2 h dvs v3
    in v4 : (adams s r b h [v1', v2', v3', applyf s r b v4] v4)
adams _ _ _ _ _ _ = error "adams"

applyf :: Float -> Float -> Float -> Vector3 Float -> Vector3 Float
applyf s r b v = Vector3 (fx s r b v) (fy s r b v) (fz s r b v)

adamsList s r b h start = let
    -- vs = take adamsK (explicitEulerList s r b h start)
    m = 10
    vs = drop (m P.- adamsK) (take m (explicitEulerList s r b h start))
    dvs = map (applyf s r b) vs
    v3 = last vs
    in vs ++ adams s r b h dvs v3

toTuple :: Vector3 a -> (a, a, a)
toTuple (Vector3 x y z) = (x, y, z)

list :: Float -> MethodType -> [(Float, Float, Float)]
list r method = take n $ map toTuple $ method s r b dt start where
    dt = 0.01
    s = 10
    b = 2.6666666
    start = Vector3 10.0 10.0 10.0

n :: Int
n = 3000

type Writer = Float -> String -> [(Float, Float, Float)] -> P.IO ()
generalOutput :: Writer -> [Float] -> String -> MethodType -> P.IO ()
generalOutput writer rs methodName method = P.mapM_ (\r -> writer r methodName (list r method)) rs

rlist :: [String] -> [Float]
rlist [] = [24.0, 28.0] --[1.0, 2.0 .. 30.0]
rlist as = map P.read as

mainGeneral :: Writer -> [Float] -> P.IO ()
mainGeneral w rs = P.mapM_ (P.uncurry (generalOutput w rs)) methods

mainG, mainF :: [Float] -> P.IO ()
mainG = mainGeneral plotX11
plotX11 :: Writer
plotX11 r methodName vs =
  do
    _ <- system "xdotool type 'exit'"
    result <- plot' [Interactive] X11 $ Data3D [Color Red, Style Lines, Title (methodName ++ " r = " ++ show r)] [] vs
--    kr <- system "xdotool type 'exit'"
    P.putStrLn (if result then "success" else "fail")

mainF = mainGeneral writeToFile
writeToFile :: Writer
writeToFile r methodName vs = P.writeFile (methodName ++ show r) $ join [printf "%f %f %f\n" x y z | (x, y, z) <- vs]

main :: P.IO ()
main = do
  args <- getArgs
  let l = rlist args in
    mainG l

methods :: [(String, MethodType)]
methods = [
  ("euler", explicitEulerList),
  ("implicit", implicitEulerList),
  ("rk", rungeKutta4List),
  ("adams", adamsList)
           ]
