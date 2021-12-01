module Day01 where

import           Advent.Parsing

{-  Counts the number of times in a list of numbers that the next number is
    larger than the current number. -}
countIncreasing :: Ord a => [a] -> Int
countIncreasing (x:y:xs) = 
  if x < y 
    then 1 + countIncreasing (y:xs) 
    else countIncreasing (y:xs)
countIncreasing _ = 0

{- Creates a new list y from the input x, where y[i] = x[i] + x[i+1] + x[i+2] -}
listOfSumOfThreeConsecutive :: [Int] -> [Int]
listOfSumOfThreeConsecutive (a:b:c:xs) = (a+b+c):listOfSumOfThreeConsecutive (b:c:xs)
listOfSumOfThreeConsecutive _ = []
   
main :: IO ()
main = do
  input <- parseLinesFromDay 1 pInt
  putStrLn $ "Part One: " ++ show (countIncreasing input)
  putStrLn $ "Part Two: " ++ show (countIncreasing $ listOfSumOfThreeConsecutive input)