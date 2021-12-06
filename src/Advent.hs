module Advent where

runNTimes :: Int -> (a -> a) -> a -> a
runNTimes 0 f x = x
runNTimes n f x = runNTimes (n-1) f (f x)