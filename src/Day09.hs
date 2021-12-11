
module Day09 where
import           Advent.Graph   (bfs)
import           Advent.Grid
import           Advent.Parsing
import qualified Data.List      as List
import qualified Data.Map       as Map
import           Data.Maybe     (isJust, isNothing, mapMaybe)
import qualified Data.Ord

type Height = Int

pGrid :: Parser (Grid Height)
pGrid = do
  rows <- pSepBy (pMany pDigit) pNewLine
  return $ Map.fromList [ ((rowNumber, colNumber), energy)  | (row, rowNumber ) <- zip rows [0..], (energy, colNumber) <- zip row [0..]]

adjacent :: Position -> [Position]
adjacent (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

isLowest :: Grid Height -> Position -> Bool
isLowest grid pos = all ( > value) adjacentValues
  where
    value = grid Map.! pos
    adjacentValues = mapMaybe (grid Map.!?) (adjacent pos)

basin :: Grid Height -> Position -> Int
basin grid = length . bfs (next grid) id
  where 
    next grid p = 
      filter(\p -> grid Map.! p /= 9) 
        $ filter(\p -> isJust $  grid Map.!? p) (adjacent p)

main :: IO()
main = do
  input <- parseFromDay 9 pGrid
  let lowest =  Map.filterWithKey (\pos _ -> isLowest input pos) input
  putStrLn $ "Part One: " ++ show (sum $ (1+) <$> Map.elems lowest)
  putStrLn $ "Part Two: " ++ show (
    product $ take 3 $ List.sortOn Data.Ord.Down $fmap (basin input) $  Map.keys lowest)


