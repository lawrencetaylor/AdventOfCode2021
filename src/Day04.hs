module Day04 where

import           Advent.Parsing
import           Control.Applicative  ((<|>))
import           Data.Map             (Map)
import qualified Data.Map             as Map
import qualified Text.Megaparsec      as T
import qualified Text.Megaparsec.Char as T
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.Foldable as Set

type Game = ([Int], [Card], [Card])

pBingo :: Parser Game
pBingo = do
  x <- T.sepBy pInt (pChar ',')
  _ <- T.newline
  _ <- T.newline
  z <- pCards
  let grids = fmap(\(grid, i) -> createCard i grid) $ zip z [0..]
  return (x, grids, [])

data Card = Card {
  i :: Int,
  numbers :: Map Int (Int,Int),
  seen    :: Set (Int, Int)
} deriving (Show, Eq)

createCard :: Int -> [[Int]] -> Card
createCard i lines = Card i grid seen
  where
    grid = toPositions lines
    n = length $ head lines
    seen = Set.empty

mark :: Int -> Card -> Card
mark n card = 
  case  (Map.!?) (numbers card) n of
    Just (x,y) -> card { seen = Set.insert (x,y) (seen card) }
    Nothing -> card

checkCell ::  Card -> (Int, Int) -> Bool
checkCell card position  = Set.member position (seen card) 

checkRow :: Card -> Int -> Bool
checkRow card y  = and $ check <$> [(x, y) | x <- [0..4]]
  where
    check = checkCell card

checkRows :: Card -> Bool
checkRows card = or $ checkRow card <$> [0..4]

checkCol :: Card -> Int -> Bool
checkCol card x  = and $ check <$> [(x, y) | y <- [0..4]]
  where
    check = checkCell card

checkCols :: Card -> Bool
checkCols card = or $ checkCol card <$> [0..4]

checkCard :: Card -> Bool
checkCard card = checkRows card || checkCols card

pCardNumber = pInt <|> (T.some $ pChar ' ') >>. pInt
pCardLine = T.some pCardNumber .>> (pNewLine <|> T.eof)
pCard = T.some pCardLine
pCards = T.sepBy pCard pNewLine

toPositions :: [[Int]] -> Map Int (Int,Int)
toPositions lines = Map.fromList $ concat $ zipWith (\y xs -> zipWith (\x v -> (v,(x,y))) [0..] xs) [0..] lines

initialiseGrid :: Int -> Map (Int, Int) Bool
initialiseGrid n = Map.fromList [ ((x,y), False) | x <- [0..n-1], y <- [0..n-1] ]

play :: Game -> (Int, Card)
play (x:xs, cards, []) = 
  case filter checkCard newCards of
    [] -> play (xs, newCards, [])
    [winner] -> (x, winner)
  where 
    markWithX = mark x
    newCards = markWithX <$> cards 


play2 :: Game -> (Int, Card)
play2 (x:xs, cards, winners) = case (newWinnersL == cardsL) of
  True -> (x, lastWinner)
  False -> play2 (xs, newCards, newWinners)
  where 
    markWithX = mark x
    newCards = markWithX <$> cards 
    newWinners = filter checkCard newCards
    newWinnersL = length newWinners
    cardsL = length cards
    isExistingWinner card = any (\c -> (i c) == (i card)) winners 
    [lastWinner] = filter (not . isExistingWinner) newWinners
    
    

unmarked :: Card -> [Int]
unmarked card = [ x | (x,y) <- Map.toList $ numbers card, not $ checkCell card y ]

main :: IO ()
main = do
  input <- parseFromDay 7 pBingo
  -- let  (x, [_,_,c3], []) = playN 12 input
  -- putStrLn $ show $ checkCell c3 (0,4)
  let (last, winner) = play input
  putStrLn $ show $ last * (sum $ unmarked winner)

  let (last2, winner2) = play2 input
  putStrLn $ show $ last2 * (sum $ unmarked winner2)
  -- putStrLn $ "Part 1: " ++ show (play input)
  -- putStrLn $ "Part 2: " ++ show (length $ filter i:L
