module Day07 where
import Advent.Parsing
import Data.Foldable (minimumBy)
import Data.Ord (comparing)

fuel :: Int -> Int -> Int
fuel x x' = abs (x-x')

fuel2 :: Int -> Int -> Int 
fuel2 x x' = y * (y+1) `div` 2
  where y = abs (x - x')

main :: IO ()
main = do
  input <- parseFromDay 7 (pSepBy pInt pComma )
  let min = minimum input
  let max = maximum input
  putStrLn $ "Part 1: " ++ show (minimum  $ fmap (\x -> sum $ fuel x <$> input) [min..max])
  putStrLn $ "Part 2: " ++ show (minimum  $ fmap (\x -> sum $ fuel2 x <$> input) [min..max])