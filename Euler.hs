module Euler where
    
import Prelude
import Data.List
import Text.Printf
import Control.Monad


data Vector3 a = Vector3 !a !a !a deriving Show

next :: Vector3 a -> (Vector3 a -> Vector3 a) -> a -> Vector3 a
next r f dt = r + dt * f r

fx, fy, fz :: a -> a -> a -> a-> Vector3 a -> a
fx t s r b (Vector3 x y z) = - s * x + s * y
fy t s r b (Vector3 x y z) = -x * z + r * x - y
fz t s r b (Vector3 x y z) = x * y - b * z

euler s r b = iterate ()  (Vector 0.0 0.0 0.0)

lenAndLast xs = (last xs, length xs)

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p [] = []
takeWhile1 p (x:xs) | p x = x : takeWhile1 p xs
                    | otherwise = [x]

newtonList :: ((Double -> Double) -> Double -> Double) -> (Double -> Double) -> (Double -> Double) -> Double -> Double -> [Double]
newtonList dumper f f' eps = takeWhile1 (not . isNaN) . map fst . takeWhile1 ((> eps) . abs . uncurry (-)) . pairwise . iterate (dumper nextx) where
    nextx :: Double -> Double
    nextx x = x - (f x) / (f' x)

newtonList' f f' eps = last . newtonList id f f' eps

newton f f' eps start = lenAndLast $ newtonList id f f' eps start
newton' f f' eps start = lenAndLast $ newtonList (\nextx x -> (x + nextx x)/ 2) f f' eps start

isFaster f f' eps x0 = (snd $ newton f f' eps x0) > (snd $ newton' f f' eps x0)

-- main = do
--   print $ newtonList' f f' eps x0 
--   -- print $ map (newtonList id f f' eps) . filter (isFaster f f' eps) $ [-10, -9.99 .. 10.0] 
--    where
--     eps = 0.0001
--     x0 = 2.0
--     f, f' :: Double -> Double
--     --f x = (x + 3) * x * (x - 2) -- x^3 + x^2 - 6x
--     --f' x = 3*x*x + 2*x - 6
--     f x = x * sin x
--     f' x = sin x + x * cos x
-- 
-- -- TODO: Rewrite using Fusion. Make it compile into cycle on Core
-- -- (-ddump-simpl) level

