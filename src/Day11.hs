module Day11 where
import           Advent.Grid
import           Advent.Parsing
import qualified Data.List       as List
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Text.Megaparsec (Pos, sepBy)

type Energy = Int

pGrid :: Parser (Grid Energy)
pGrid = do
  rows <- sepBy (pMany pDigit) pNewLine
  return $ Map.fromList [ ((rowNumber, colNumber), energy)  | (row, rowNumber ) <- zip rows [0..], (energy, colNumber) <- zip row [0..]]

step :: Position -> Grid Energy -> Grid Energy
step pos grid = case Map.lookup pos grid of
  Just 10 -> grid
  Just 9 ->
      let grid2 = Map.insert pos 10 grid
      in foldr step grid2 (neighbours pos)
  Just x -> Map.insert pos (x + 1) grid
  _ -> grid

stepAndFlash :: Grid Energy -> Grid Energy
stepAndFlash input = 
  Map.map (\x -> if x == 10 then 0 else x) $ 
  foldr step input (Map.keys input)

isSynronised :: Grid Energy -> Bool
isSynronised grid = all (== 0) $ Map.elems grid

main :: IO ()
main = do
  input <- parseFromDay 11 pGrid
  let sequence = iterate stepAndFlash input
  putStrLn $ "Part One: " ++ show (sum $ length . Map.filter (0 ==)  <$> take 101 sequence)
  putStrLn $ "Part Two: " ++ show (fst . head $ filter (\(i, g) -> isSynronised g) $ zip [0..] sequence)