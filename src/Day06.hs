module Day06 where

import           Advent
import           Advent.Parsing
import qualified Data.List      as List
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Maybe     (fromMaybe)

type Fish = Map Int Int

tickFish :: Fish -> Fish
tickFish fish = Map.insert 8 zeros $ Map.mapKeysWith (+) dec fish
  where
    zeros = fromMaybe 0 (fish Map.!? 0)

    dec 0 = 6
    dec n = n - 1

countFish :: Fish -> Int -> Int
countFish fish n =
  sum $
  fmap snd $
  Map.toList $ runNTimes n tickFish fish

main :: IO ()
main = do
  input <- parseFromDay 6 (pSepBy pInt pComma )
  let fish = Map.fromList $ fmap (\l@(x:xs) -> (x, length l)) $ List.group $ List.sort input
  putStrLn $ "Part 1: " ++ show (countFish fish 80)
  putStrLn $ "Part 1: " ++ show (countFish fish 256)

