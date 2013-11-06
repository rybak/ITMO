{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeOperators, MultiParamTypeClasses, RankNTypes, UnboxedTuples, FunctionalDependencies, RecordWildCards #-}
 
import Prelude ( Show(..)
               , error, undefined
               , Bool(..), (&&), (||), not
               , Int
               , String
               , Float, Double
               , fromIntegral, realToFrac
               , pi, sin, cos, asin, acos, atan, atan2, sqrt
               , Monad(..) )
import qualified Prelude as P
 
import Control.Monad.Trans (liftIO)
import Data.IORef
import qualified Data.Set as S
import Data.Time.Clock.POSIX
import Graphics.UI.Gtk hiding (drawPolygon,Arrow)
import Graphics.UI.Gtk.Gdk.Events
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as M
import System.IO.Unsafe (unsafePerformIO)
 

-- Rybak imports
import Text.Printf
import Data.List
import Control.Monad hiding(mzero)
import Data.Binary.IEEE754
import Data.Binary
import System.Environment(getArgs)

-- Holes
 
data Hole = Hole
hole = error "hole"
 
-- Algebra
 
class Monoid a where
  mzero   :: a
  mappend :: a -> a -> a
  mconcat :: [ a ] -> a
  mconcat = P.foldr mappend mzero
 
-- Instances
 
newtype Any = Any { unAny :: Bool }
 
instance Monoid Any where
  mzero = Any False
  mappend (Any x) (Any y) = Any (x || y)
 
newtype All = All { unAll :: Bool }
 
instance Monoid All where
  mzero = All True
  mappend (All x) (All y) = All (x && y)
 
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
 
--instance (Ring s, BasicVector v) => Ring (v s) where
--  rone = diagonal rone
--  rmult = vzip rmult
--
--instance (Field s, BasicVector v) => Field (v s) where
--  recip = vmap recip
--  (/) = vzip (/)
 
-- pointwise product
(.*) :: (Field s, BasicVector v) => v s -> v s -> v s
(.*) x y = vzip (*) x y
 
-- inner product
(**) :: (Field s, BasicVector v) => v s -> v s -> s
(**) x y = vfold (+) (x .* y)
 
norm :: (Field s, BasicVector v, P.Floating s) => v s -> s
norm v = sqrt $ v ** v
 
normalize v = v ^/ (norm v)
 
-- Matices
 
type Matrix = M.Matrix
 
instance Monoid Matrix where
  mzero = M.Matrix 0 0 0 0 0 0
  mappend = (P.+)
 
instance Group Matrix where
  ginv = negate
 
instance VectorSpace Matrix Double where
  (^*) = M.scalarMultiply
 
instance Ring Matrix where
  rone = M.identity
  rmult = (P.*)
 
instance Field Matrix where
  recip = M.invert
 
rotationMatrix a = M.Matrix (cos a) (negate $ sin a) (sin a) (cos a) 0 0
 
-- 2D Vectors
 
data Vector2 a = Vector2 !a !a
                deriving Show
 
 
uncurryVector2 f (Vector2 x y) = f x y
 
instance BasicVector Vector2 where
  diagonal s = Vector2 s s
  vmap f (Vector2 x y) = Vector2 (f x) (f y)
  vzip f (Vector2 x y) (Vector2 x' y') = Vector2 (f x x') (f y y')
  vfold o (Vector2 x y) = x `o` y
 
angleVector2 (Vector2 x y) = atan2 y x
 
angleBetweenVector2 p t = angleVector2 p - angleVector2 t
 
transformVector2 m (Vector2 x y) = Vector2 a b where
  (a, b) = M.transformPoint m (x, y)
 
-- 3D Vectors
 
data Vector3 a = Vector3 !a !a !a
                deriving Show
 
instance BasicVector Vector3 where
  diagonal s = Vector3 s s s
  vmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)
  vzip f (Vector3 x y z) (Vector3 x' y' z') = Vector3 (f x x') (f y y') (f z z')
  vfold o (Vector3 x y z) = x `o` y `o` z
 
--
 
data Color3 a = Color3 !a !a !a
                deriving Show
 
instance BasicVector Color3 where
  diagonal s = Color3 s s s
  vmap f (Color3 x y z) = Color3 (f x) (f y) (f z)
  vzip f (Color3 x y z) (Color3 x' y' z') = Color3 (f x x') (f y y') (f z z')
  vfold o (Color3 x y z) = x `o` y `o` z
 
--
 
data Color4 a = Color4 !a !a !a !a
                deriving Show
 
instance BasicVector Color4 where
  diagonal s = Color4 s s s s
  vmap f (Color4 x y z a) = Color4 (f x) (f y) (f z) (f a)
  vzip f (Color4 x y z a) (Color4 x' y' z' a') = Color4 (f x x') (f y y') (f z z') (f a a')
  vfold o (Color4 x y z a) = x `o` y `o` z `o` a
 
-- Function
 
infixr 0 $
 
($) :: (a -> b) -> a -> b
($) f x = f x
 
-- Category and Arrow
 
class Category cat where
  id  :: cat a a
  (.) :: cat b c -> cat a b -> cat a c
 
(>>>) :: Category cat => cat a b -> cat b c -> cat a c
(>>>) = P.flip (.)
 
instance Category (->) where
  id x = x
  (.) f g x = f (g x)
 
class Category cat => Arrow cat where
  arr    :: (a -> b) -> cat a b
  first  :: cat a b -> cat (a, c) (b, c)
  second :: cat a b -> cat (c, a) (c, b)
  (***)  :: cat a b -> cat a' b' -> cat (a, a') (b, b')
  (***) f g = first f >>> second g
  (&&&)  :: cat a b -> cat a b' -> cat a (b, b')
  (&&&) f g = arr (\x -> (x, x)) >>> (f *** g)
 
instance Arrow (->) where
  arr = id
  first  f (x, y)  = (f x, y)
  second f (x, y)  = (x, f y)
  (***) f g (x, y) = (f x, g y)
  (&&&) f g x      = (f x, g x)
 
-- Functor
 
class Functor f where
  fmap :: (a -> b) -> f a -> f b
 
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

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 p (x:xs) | p x = x : takeWhile1 p xs
                    | P.otherwise = [x]

notNaN :: Vector3 Float -> Bool
notNaN (Vector3 x y z) = P.not ((P.isNaN x) P.|| (P.isNaN y) P.|| (P.isNaN z))

explicitEuler, implicitEuler, rungeKutta4 :: Float -> Float -> Float -> Float -> Vector3 Float -> Vector3 Float
explicitEuler s r b dt v@(Vector3 x y z) = Vector3 (x + dt * (dx v)) (y + dt * (dy v)) (z + dt * (dz v)) where
                dx = fx s r b
                dy = fy s r b
                dz = fz s r b

implicitEuler s r b h = (oneIter s r b h) . (oneIter s r b h) . (explicitEuler s r b h)
                        where
    oneIter s r b h (Vector3 x y z) = Vector3
        ((x + h * s * y) / (1.0 + s * h))
        ((y + h * x * (r - z)) / (1.0 + h))
        ((h * x * y + z) / (1.0 + b * h))
 
rungeKutta4 s r b h v = let
  f v@(Vector3 x y z) = Vector3 (fx s r b v)(fy s r b v)(fz s r b v)
  k1 = h ^* f v
  k2 = h ^* f (v + 0.5 ^* k1)
  k3 = h ^* f (v + 0.5 ^* k2)
  k4 = h ^* f (v + k3)
  d = (1.0 / 6.0) ^* (k1 + 2 ^* k2 + 2 ^* k3 + k4)
  in v + d
  
lorenz :: (Float -> Float -> Float -> Float -> (Vector3 Float) -> Vector3 Float) -> Float -> Float -> Float -> Float -> (Vector3 Float) -> [Vector3 Float]
lorenz next s r b h start = takeWhile1 notNaN $ P.iterate (next s r b h) start

explicitEulerList, implicitEulerList, rungeKutta4List, adamsList :: Float -> Float -> Float -> Float -> Vector3 Float -> [Vector3 Float]
explicitEulerList = lorenz explicitEuler
implicitEulerList = lorenz implicitEuler
rungeKutta4List = lorenz rungeKutta4

adamsK = 4

adamsOne :: Float -> [Float] -> Float -> Float
adamsOne h [y3', y2', y1', y0'] yi = yi + (h / 24.0) * (55 * y0' - 59 * y1' + 37 * y2' - 9 * y3')
adamsOne _ _ _ = error "adamsOne"

adams :: Float -> Float -> Float -> Float -> [Vector3 Float] -> Vector3 Float -> [Vector3 Float]
adams s r b h [v3, v2', v1', v0'] v = let
    nextv = v
    in nextv : (adams s r b h [v2', v1', v0', applyf s r b v] v)
adams _ _ _ _ _ _ = error "adams method"

applyf :: Float -> Float -> Float -> Vector3 Float -> Vector3 Float
applyf s r b v = Vector3 (fx s r b v) (fy s r b v) (fz s r b v)

adamsList s r b h start = let
    sl = map (applyf s r b) (take adamsK (rungeKutta4List s r b h start))
    v4 = last sl
    in adams s r b h sl v4

toTuple (Vector3 x y z) = (x, y, z)

list r method = take n $ map toTuple $ method s r b dt start where
    dt = 0.005
    s = 10
    b = 2.6666666
    start = Vector3 3.051522 1.582542 15.62388

n = 20000

output filename method = P.mapM_ (\r -> P.writeFile (filename ++ show r) $ join [printf "%f %f %f\n" x y z | (x, y, z) <- list r method])

main :: P.IO ()
main = do
    argv <- getArgs
    let l = if null argv then [1.0, 2.0 .. 30.0] else map P.read argv in
      output "euler" explicitEulerList l
      >> output "implicit" implicitEulerList l
      >> output "rk" rungeKutta4List l
