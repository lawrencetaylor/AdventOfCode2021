module Day05 where

import           Advent.Parsing
import           Control.Applicative ((<|>))
import qualified Data.List           as List
import           Data.Map            (Map)
import qualified Data.Map            as Map

type Position = (Int, Int)
type VentLine = (Position, Position)

pVent :: Parser VentLine
pVent = do
  x1 <- pInt .>> pChar ','
  y1 <- pInt .>> pString " -> "
  x2 <- pInt .>> pChar ','
  y2 <- pInt .>> (pNewLine <|> pEof)
  return ((x1, y1), (x2, y2))

type Floor = Map Position Int

alongOrUp :: VentLine -> [Position]
alongOrUp ((x1, y1), (x2, y2))
  | (x1 == x2) || (y1 == y2) = [(x, y) | x <- [x1'..x2'], y <- [y1'..y2']]
  | otherwise = []
  where
    x1' = min x1 x2
    x2' = max x1 x2
    y1' = min y1 y2
    y2' = max y1 y2

diagonal :: VentLine -> [Position]
diagonal ((x1, y1), (x2, y2))
  | (x1 == x2) || (y1 == y2) = []
  | otherwise = iter (x1, y1)
  where
    stepX = if x1 < x2 then 1 else -1
    stepY = if y1 < y2 then 1 else -1

    iter (x,y)
      | x == x2 && y == y2 = [(x2,y2)]
      | otherwise = (x,y) : iter (x+stepX, y+stepY)


markVents :: (VentLine -> [Position]) -> VentLine -> Floor -> Floor
markVents vents ventLine floor = foldr addVent floor (vents ventLine)
  where
    addVent vent = Map.insertWith (+) vent 1

overlaps :: (VentLine -> [Position]) -> [VentLine] -> Int
overlaps vents lines = length duplicates
  where
    floor = foldr (markVents vents) Map.empty lines
    duplicates = Map.filter (> 1) floor

main :: IO ()
main = do
  input <- parseFromDay 5 (pMany pVent)
  putStrLn $ "Part One: " ++ show (overlaps alongOrUp input)
  let withDiagonal line = diagonal line ++ alongOrUp line
  putStrLn $ "Part Two: " ++ show (overlaps withDiagonal input)
