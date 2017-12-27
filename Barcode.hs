import Data.Array (Array(..), (!), bounds, elems, indices, ixmap, listArray)
import Control.Arrow ((&&&))
import Data.Function (on)
import Text.ParserCombinators.Parsec
import Control.Monad.State
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

type Pixel = Char
type RGB = (Pixel, Pixel, Pixel)
type Pixmap = Array (Int, Int) RGB
type PixParser a = GenParser Char () a

parseRawPPM :: PixParser Pixmap
parseRawPPM = do
  header <- many (noneOf "\n")
  spaces1
  when (header /= "P6") (fail "invalid raw header")
  width <- parseInt -- :: Int
  spaces1
  height <- parseInt
  spaces1
  maxValue <- parseInt
  when (maxValue /= 255) (fail "max value out of spec")
  parseByte
  pxs <- count (width * height) parseRGB
  return (listArray ((0,0), (width-1, height-1)) pxs)

parseRGB :: PixParser RGB
parseRGB = do
  r <- parseByte
  g <- parseByte
  b <- parseByte
  return (r,g,b)

parseByte :: PixParser Pixel
parseByte = anyChar 

parseInt :: PixParser Int
parseInt = read <$> many1 digit

spaces1 :: PixParser ()
spaces1 = skipMany1 space

--converting to greyscale
type Pixel2 = Int
luminance :: RGB -> Pixel2
luminance (r,g,b) = round (r' * 0.30 + g' * 0.59 + b' * 0.11) where
  [r',g',b'] = map (fromIntegral . fromEnum) [r, g, b]

type Greymap = Array (Int, Int) Pixel2
pixmapToGreymap :: Pixmap -> Greymap
pixmapToGreymap = fmap luminance

data Bit = Zero | One deriving (Eq, Show)

--converting a pixel to either black or white
threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold n a = binary `fmap` a where
  binary i | i < pivot = Zero
           | otherwise = One
  pivot = round $ least + (greatest - least) * n
  least = fromIntegral $ choose (<) a
  greatest = fromIntegral $ choose (>) a
  choose f = foldA1 $ \x y -> if f x y then x else y

--run-length encoding to normalize bar thickness from image file
type Run = Int
type RunLength a = [(Run, a)]

runLength :: Eq a => [a] -> RunLength a
runLength = map (length &&& head) . group where

runLengths = map fst . runLength

type Score = Ratio Int

scaleToOne :: [Run] -> [Score]
scaleToOne xs = map divide xs where
  divide d = fromIntegral d / divisor
  divisor = fromIntegral (sum xs)

type ScoreTable = [[Score]]

--asSRL converts code table into ratio table
asSRL :: [String] -> ScoreTable
asSRL = map (scaleToOne . runLengths)

leftOddSRL = asSRL leftOddList
leftEvenSRL = asSRL leftEvenList
rightSRL = asSRL rightList
paritySRL = asSRL parityList

--comparing accuracies between 2 run encodings (scaled)
distance :: [Score] -> [Score] -> Score
distance a b = sum . map abs $ zipWith (-) a b

type Digit = Word8
bestScores :: ScoreTable -> [Run] -> [(Score, Digit)]
bestScores srl ps = take 3 . sort $ scores where
  scores = zip [distance d (scaleToOne ps) | d <- srl] [0..9]

--remembering parity
data Parity a = Even a | Odd a | None a deriving (Show)

fromParity :: Parity a -> a
fromParity (Even a) = a
fromParity (Odd a) = a
fromParity (None a) = a

parityMap :: (a -> b) -> Parity a -> Parity b
parityMap f (Even a) = Even (f a)
parityMap f (Odd a) = Odd (f a)
parityMap f (None a) = None (f a)

instance Functor Parity where
  fmap = parityMap

compareWithoutParity = compare `on` fromParity

bestLeft :: [Run] -> [Parity (Score, Digit)]
bestLeft ps = sortBy compareWithoutParity
              ((map Odd (bestScores leftOddSRL ps)) ++
              (map Even (bestScores leftEvenSRL ps)))

bestRight :: [Run] -> [Parity (Score, Digit)]
bestRight = map None . bestScores rightSRL

--chunking list into runs for each digit
chunkWith :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chunkWith _ [] = []
chunkWith f xs = let (h, t) = f xs in
  h : chunkWith f t

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = chunkWith (splitAt n)

candidateDigits :: RunLength Bit -> [[Parity Digit]]
candidateDigits ((_, One):_) = []
candidateDigits rle | length rle < 59 = []
candidateDigits rle
  | any null match = []
  | otherwise = map (map (fmap snd)) match
  where match = map bestLeft left ++ map bestRight right
        left = chunksOf 4 . take 24 . drop 3 $ runLengths
        right = chunksOf 4 . take 24 . drop 32 $ runLengths
        runLengths = map fst rle
      
