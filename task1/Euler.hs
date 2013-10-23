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
import Graphics.UI.Gtk hiding (fill,drawPolygon,lineWidth,Arrow)
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

-- Something scary
 
data Corner = TopLeft | TopCenter | TopRight
            | MiddleLeft | MiddleCenter | MiddleRight
            | BottomLeft | BottomCenter | BottomRight
            deriving Show
 
cornerPos :: Corner -> Vector2 Double
cornerPos TopLeft      = Vector2 0 0
cornerPos TopCenter    = Vector2 (1/2) 0
cornerPos TopRight     = Vector2 1 0
cornerPos MiddleLeft   = Vector2 0 (1/2)
cornerPos MiddleCenter = Vector2 (1/2) (1/2)
cornerPos MiddleRight  = Vector2 1 (1/2)
cornerPos BottomLeft   = Vector2 0 1
cornerPos BottomCenter = Vector2 (1/2) 1
cornerPos BottomRight  = Vector2 1 1
 
data Image = Image M.Matrix Pixbuf
 
{-# NOINLINE image #-}
image w h flip c x = unsafePerformIO $ do
  i <- pixbufNewFromFile x
  r <- pixbufScaleSimple i w h InterpHyper
  return $ Image (M.Matrix 1 0 0 (if flip then -1 else 1) (negate a) (if flip then b else negate b)) r
  where
  (Vector2 a b) = vzip (*) (Vector2 (fromIntegral w) (fromIntegral h)) (cornerPos c)
 
putImage (Image m i) = do
  C.save
  C.transform m
  setSourcePixbuf i 0 0
  C.paint
  C.restore
 
render :: (Show z) => Bool
       -> Int -> Int -> Int -> z
       -> (Double -> Double -> (String -> Bool) -> z -> z)
       -> (Vector2 Double -> z -> C.Render ())
       -> P.IO ()
render debug width height tick z stateFun renderFun = do
  t0 <- getPOSIXTime
  prevtime <- newIORef t0
  state <- newIORef z
  keys <- newIORef (S.empty :: S.Set String)

  initGUI
  window <- windowNew
  drawingArea <- drawingAreaNew
  set window [ containerChild := drawingArea ]
  windowSetDefaultSize window width height
  widgetModifyBg drawingArea StateNormal (Color 0xffffff 0xffffff 0xffffff)
  onDestroy window mainQuit

  onExpose drawingArea $ handleDraw drawingArea state
  onKeyPress window $ handleKey t0 prevtime keys state
  onKeyRelease window $ handleKey t0 prevtime keys state
  timeoutAdd (updateState t0 prevtime keys state >> widgetQueueDraw drawingArea >> return True) tick

  widgetShowAll window
  mainGUI
  where
  updateState t0 prevtime keys state = do
    t <- getPOSIXTime
    tp <- readIORef prevtime
    ks <- readIORef keys
    cs <- readIORef state
    if debug
       then do
            P.putStrLn "-- Debug"
            P.print t
            P.print ks
            P.print cs
       else return ()
    writeIORef state $ stateFun (P.realToFrac $ t P.- t0) (P.realToFrac $ t P.- tp) (`S.member` ks) cs
    writeIORef prevtime t
  handleDraw drawingArea state _ = do
    win <- widgetGetDrawWindow drawingArea
    (w', h') <- widgetGetSize drawingArea
    let w = realToFrac w'
        h = realToFrac h'
    cs <- readIORef state
    renderWithDrawable win $ do
      C.setMatrix $ M.Matrix 1 0 0 (-1) 0 h
      renderFun (Vector2 w h) cs
    return True
  handleKey t0 prevtime keys state (Key rel _ _ mod _ _ _ val name char) = do
    modifyIORef keys $ \ks ->
      if rel
         then S.delete name ks
         else S.insert name ks
    updateState t0 prevtime keys state
    return True
 
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
 
-- drawing
 
rgb (Color3 r g b) = C.setSourceRGB r g b
rgba (Color4 r g b a) = C.setSourceRGBA r g b a
 
translate = uncurryVector2 C.translate
rotate = C.rotate
lineWidth = C.setLineWidth
scale = uncurryVector2 C.scale
moveTo = uncurryVector2 C.moveTo
lineTo = uncurryVector2 C.lineTo
 
drawPolygon xs = drawOpenPolygon xs >> C.closePath
drawOpenPolygon [] = return ()
drawOpenPolygon (x:xs) = C.newPath >> moveTo x >> P.mapM_ lineTo xs
 
fill f = f >> C.fill
stroke f = f >> C.stroke
 
fillPolygon = fill . drawPolygon
strokePolygon = stroke . drawPolygon
strokeLine = stroke . drawOpenPolygon
strokeWidthLine x p = lineWidth x >> strokeLine p
 
--
 
data RocketState = RocketState
  { t  :: Double
  , p  :: Vector2 Double
  , v  :: Vector2 Double
  , o  :: Vector2 Double
  , a  :: Vector2 Double
  } deriving Show
 
startState = RocketState
  { t = 0
  , p = Vector2 0 0
  , v = Vector2 0 0
  , o = Vector2 1 0
  , a = Vector2 0 0
  }
 
throttle k s = s { a = 100 ^* c ^* (o s)
                 , o = transformVector2 (rotationMatrix $ 0.01 * alpha) (o s) } where
  c = key "Down" (- 1) 0 + key "Up" 1 0
  alpha = key "Left" (- 1.4) 0 + key "Right" 1.4 0
  key s yes no = if k s then yes else no
 
physics dt s = s { p = p s + dt ^* v s
                 , v = v s + dt ^* a s }
 
friction dt s = s { v = v s - dt ^* v s ^/ 2 }
 
stateFun t dt keyChecker = friction dt . physics dt . throttle keyChecker . (\x -> x { t = t })
 
wbushes w f 0 _ _ = []
wbushes w f n p seg = Vector2 p nextp
                 : wbushes (w ^/ 1.1) f (n P.- 1) nextp (nextseg f)
              P.++ wbushes (w ^/ 1.1) f (n P.- 1) nextp (nextseg (negate . f))
  where
  nextp = p + seg + w
  nextseg f = transformVector2 (rotationMatrix $ f n) seg
 
renderFun window (RocketState {..}) = do
  translate $ (Vector2 (1/2) (1/2)) .* window
 
  rgb (Color3 0 0 0)
  C.rectangle (-5) (-5) 10 10
  C.fill
 
  P.mapM_ (\(Vector2 f t) -> strokeWidthLine 1 [f, t]) $ wbushes (Vector2 (2 * sin t) 0) ((* 0.05) . fromIntegral) 10 (Vector2 0 0) (Vector2 0 20)
 
  translate p
  C.rotate $ angleBetweenVector2 o (Vector2 0 1)
  putImage rocket
  where
  rocket = image 50 50 True MiddleCenter "rocket.png"
 
-- main = render True 500 500 16 startState stateFun renderFun
fx, fy, fz :: (Monoid a, Group a, Ring a) => a -> a -> a-> Vector3 a -> a
fx s _ _ (Vector3 x y z) =  s * (y - x)
fy _ r _ (Vector3 x y z) =  x * (r - z) - y
fz _ _ b (Vector3 x y z) =  x * y - b * z

pairwise :: [a] -> [(a,a)]
pairwise (x1:x2:xs) = (x1,x2) : pairwise (x2:xs)
pairwise _ = []

toList (Vector3 a b c) = [a, b, c]

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p [] = []
takeWhile1 p (x:xs) | p x = x : takeWhile1 p xs
                    | P.otherwise = [x]

diff :: (P.Ord a, Group a, Monoid a, P.Num a) => Vector3 a -> Vector3 a -> a
diff v1 v2 = P.maximum $ P.map P.abs $ P.zipWith (-) (toList v1) (toList v2)

notNaN (Vector3 x y z) = P.not ((P.isNaN x) P.|| (P.isNaN y) P.|| (P.isNaN z))

goodDiff :: Float -> (Vector3 Float, Vector3 Float) -> Bool
goodDiff eps (v1, v2) = diff v1 v2 P.> eps
                        P.&& (notNaN v1) P.&& (notNaN v2)
                              

eulerList :: Float -> Float -> Float -> Float -> Float -> Vector3 Float -> [(Vector3 Float, Vector3 Float)]
eulerList s r b eps dt start = takeWhile1 (goodDiff eps) $ pairwise $ P.iterate f start
              where
                f v@(Vector3 x y z) = Vector3 (x + dt * (dx v)) (y + dt * (dy v)) (z + dt * (dz v))
                                      
                dx = fx s r b
                dy = fy s r b
                dz = fz s r b

eulerList2 :: Float -> Float -> Float -> Float -> Float -> Vector3 Float -> [Vector3 Float]
eulerList2 s r b _ dt start = takeWhile1 notNaN $ P.iterate f start
                        where
                f v@(Vector3 x y z) = Vector3 (x + dt * (dx v)) (y + dt * (dy v)) (z + dt * (dz v))
                                      
                dx = fx s r b
                dy = fy s r b
                dz = fz s r b


fromELtoDL :: [(Vector3 Float, Vector3 Float)] -> [(Float, Float, Float)]

fromELtoDL = P.map (toTuple . P.fst)

toTuple (Vector3 x y z) = (x,y,z)

fst3 (a, _, _) = a
snd3 (_, b, _) = b
trd3 (_, _, c) = c

filename = "euler.bin"

list r = take n $ map toTuple $ eulerList2 s r b eps dt start
   where
     eps = 0.0001 :: Float
     dt = 0.005
     s = 10
     b = 2.6666666
--     b = 8.0 / 3.0 :: Float
     start = Vector3 3.051522 1.582542 15.62388

n = 10000

main2 = encodeFile filename $ list 28

main1 =
    P.writeFile filename $ join [printf "%f %f %f\n" x y z | (x, y, z) <- list 28] 


main = do
    argv <- getArgs
    let l = map P.read argv in
        P.mapM_ (\r ->
            P.writeFile (filename ++ show r) $ join [printf "%f %f %f\n" x y z | (x, y, z) <- list r]
            ) l
 
