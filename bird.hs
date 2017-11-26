import Data.List
import Control.Monad

--Ch 2

--Ex E

first :: (a -> Bool) -> [a] -> Maybe a
first p xs
  | null fil = Nothing
  | otherwise = Just (head fil)
  where fil = filter p xs

--Ex F
exp' :: Integer -> Integer -> Integer
exp' x n
  | n == 0 = 1
  | n == 1 = x
  | even n = res * res
  | otherwise = x * exp' x (n-1)
  where res = exp' x (n `div` 2)

exp2 :: (Fractional a, Integral b) => a -> b -> a
exp2 x n
  | n < 0 = 1 / exp2 x (abs n)
  | n == 0 = 1
  | n == 1 = x
  | even n = res * res
  | otherwise = x * exp2 x (n-1)
  where res = exp2 x (div n 2)
