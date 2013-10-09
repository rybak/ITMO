import Prelude
import Data.List
import Text.Printf
import Control.Monad

filename :: String
filename = "euler"

toPairs :: [a] -> [(a,a)]
toPairs (a:b:xs) = (a,b) : toPairs xs
toPairs [] = []
mutationProbability :: (String, String) -> Double
mutationProbability (a, b) = foldl (\x y -> x * (if y then q else p)) 1.0 $ zipWith (==) a b
    where
        p = 1.0 / genericLength a
        q = 1.0 - p

next :: a -> a3 -> (a -> a3 -> a3) -> a3
next t y f dt = y + dt * f t y

-- TODO : add vector3 from rocket

infixr 4 +
infixr 4 -
infixr 5 ^*
infixr 5 ^/
infixr 5 *
infixr 5 /
 
data Vector3 a = Vector3 !a !a !a
                 deriving show

fx, fy, fz :: Double -> a -> a
fx t (Vector3 x y z) = - sigma * x + sigma * y
fy 

main :: IO ()
main = do
    input <- readFile $ filename ++ ".in"
    writeFile (filename ++ ".out") $ join [printf "%.20f\n" a | a <- map mutationProbability  (toPairs . tail . lines) input]
