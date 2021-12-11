module Advent.Grid where

import qualified Data.List as List
import           Data.Map  (Map)
import qualified Data.Map  as Map

type Position = (Int, Int)
type Grid a = Map Position a

neighbours :: Position -> [Position]
neighbours (x, y) =
  [ (x + dx, y + dy) | dx <- [-1..1]
                      , dy <- [-1..1]
                      , dx /= 0 || dy /= 0 ]

printGrid :: (Show a ) => Grid a -> IO ()
printGrid grid = do
  let positions = Map.keys grid
  let minX = minimum $ map fst positions
  let maxX = maximum $ map fst positions
  let minY = minimum $ map snd positions
  let maxY = maximum $ map snd positions
  let y = List.intercalate "\n" $ fmap (\x -> concat [ show $ grid Map.! (x, y) | y <- [minY..maxY]] ) [minX..maxX]
  putStrLn y
  putStrLn ""
