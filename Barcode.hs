import Data.Array (Array(..), (!), bounds, elems, indices, ixmap, listArray)
import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Char (digitToInt)
import Data.Ix (Ix(..))
import Data.List (foldl', group, sort, sortBy, tails)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio (Ratio)
import Data.Word (Word8)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

checkDigit :: [Int] -> Int
checkDigit digits = 10 - (sumWeight `mod` 10) where
  sumWeight = sum $ mapEveryOther digits

mapEveryOther :: [Int] -> [Int]
mapEveryOther = zipWith ($) (cycle [(*1), (*3)])

leftOddList = ["0001101", "0011001", "0010011", "0111101", "0100011",
               "0110001", "0101111", "0111011", "0110111", "0001011"]

rightList = map complement <$> leftOddList where
  complement '0' = '1'
  complement '1' = '0'

leftEvenList = map reverse rightList

parityList = ["111111", "110100", "110010", "110001", "101100",
              "100110", "100011", "101010", "101001", "100101"]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, length xs - 1) xs

leftOddCodes, leftEvenCodes, rightCodes, parityCodes :: Array Int String

leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList
parityCodes = listToArray parityList

-- | Strict left fold for arrays
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f s a = go s (indices a) where
  go s (j:js) = let s' = f s (a ! j) in
                s' `seq` go s' js
  go s [] = s

foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a

--Exercises
fourTuple :: (a, a, a, a) -> Int -> a
fourTuple (a,_,_,_) 0 = a
fourTuple (_,a,_,_) 1 = a
fourTuple (_,_,a,_) 2 = a
fourTuple (_,_,_,a) 3 = a

-- | Encoding
encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

encodeDigits :: [Int] -> [String]
encodeDigits s@(first:rest) =
  outerGuard : lefties ++ centerGuard : righties ++ [outerGuard] where
  (left, right) = splitAt 5 rest
  lefties = zipWith leftEncode (parityCodes ! first) left
  righties = map rightEncode (right ++ [checkDigit s])

leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String
rightEncode = (rightCodes !)

outerGuard = "101"
centerGuard = "01010"
