module Day03 where

import           Advent.Parsing
import           Control.Applicative (Applicative (..), (<|>))
import           Control.Applicative hiding (many, some)
import           Data.Char
import           Data.Functor        (($>))
import           Data.List           (maximumBy, minimumBy)
import           Data.Ord            (comparing)
import qualified Text.Megaparsec     as T

type Bit = Int
type Binary = [Bit]

pBit :: Parser Binary
pBit = T.many ((pChar '1' $> 1) <|> (pChar '0' $> 0))

toInt :: Binary -> Int
toInt = foldl (\acc x -> acc * 2 + x) 0

count :: [Bit] -> Int -> Int
count items i = length $ filter (== i) items

bitsInPosition :: Int -> [Binary] -> [Bit]
bitsInPosition n = fmap (!! n)

bitsInPosition' :: [Binary] -> [[Bit]]
bitsInPosition' items = (`bitsInPosition` items) <$> [0..n-1]
  where
    n = length $ head items

mostCommon :: [Bit] -> Bit
mostCommon items = maximumBy (comparing $ count items) [0,1]

leastCommon :: [Bit] -> Bit
leastCommon items = minimumBy (comparing $ count items) [0,1]

powerConsumption :: [Binary] -> Int
powerConsumption items = gamma * epsilon
  where
    positions =  bitsInPosition' items
    gamma = toInt $ mostCommon <$> positions
    epsilon = toInt $ leastCommon <$> positions

oxRating :: Int -> [Binary] -> Int
oxRating _ [x]   = toInt x
oxRating i items = oxRating (i+1) remaining
  where
    mostCommonBit = mostCommon $ bitsInPosition i items
    remaining = filter (\item -> mostCommonBit == (item !! i)) items

co2ScrubberRating :: Int -> [[Int]] -> Int
co2ScrubberRating _ [x]   = toInt x
co2ScrubberRating i items = co2ScrubberRating (i+1) remaining
  where
    leastCommonBit = leastCommon $ bitsInPosition i items
    remaining = filter (\item -> leastCommonBit == (item !! i)) items

lifeSupportRating :: [Binary] -> Int
lifeSupportRating items = oxRating' * co2ScrubberRating'
  where
    oxRating' = oxRating 0 items
    co2ScrubberRating' = co2ScrubberRating 0 items

main :: IO ()
main = do
  input <- parseLinesFromDay 3 pBit
  putStrLn $ "Part One: " ++ show (powerConsumption input)
  putStrLn $ "Part Two: " ++ show (lifeSupportRating input)
