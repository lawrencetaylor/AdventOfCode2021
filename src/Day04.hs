{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day04 where

import           Advent.Parsing
import           Control.Applicative  ((<|>))
import qualified Data.Foldable        as Set
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set
import qualified Text.Megaparsec      as T
import qualified Text.Megaparsec.Char as T

data Card = Card {
  i       :: Int,
  numbers :: Map Int (Int,Int),
  seen    :: Set (Int, Int)
} deriving (Show, Eq)

type Game = ([Int], [Card], [Card])

pBingo :: Parser Game
pBingo = do
  let pCardNumber = pInt <|> pOneOrMore ( pChar ' ') >>. pInt
  let pCardLine = T.some pCardNumber .>> (pNewLine <|> pEof)
  let pCard = T.some pCardLine
  let pCards = T.sepBy pCard pNewLine
  x <- pSepBy pInt (pChar ',')
  _ <- T.newline
  _ <- T.newline
  z <- pCards
  let grids = (\(grid, i) -> createCard i grid) <$> zip z [0..]
  return (x, grids, [])

createCard :: Int -> [[Int]] -> Card
createCard i lines = Card i grid seen
  where
    grid = Map.fromList $
            concat $ 
            zipWith (\y xs -> zipWith (\x v -> (v,(x,y))) [0..] xs) [0..] lines
    n = length $ head lines
    seen = Set.empty

mark :: Int -> Card -> Card
mark n card =
  case  (Map.!?) (numbers card) n of
    Just (x,y) -> card { seen = Set.insert (x,y) (seen card) }
    Nothing    -> card

checkCell ::  Card -> (Int, Int) -> Bool
checkCell card position  = Set.member position (seen card)

checkCard :: Card -> Bool
checkCard card = checkRows || checkCols
  where
    checkCell position  = Set.member position (seen card)
    checkRow y  = and $ checkCell <$> [(x, y) | x <- [0..4]]
    checkCol x  = and $ checkCell <$> [(x, y) | y <- [0..4]]
    checkRows = or $ checkRow <$> [0..4]
    checkCols = or $ checkCol <$> [0..4]

playToFirstWinner :: Game -> (Int, Card)
playToFirstWinner (x:xs, cards, _) =
  case filter checkCard newCards of
    []       -> playToFirstWinner (xs, newCards, [])
    [winner] -> (x, winner)
    _ -> error "Multiple winners"
  where
    markWithX = mark x
    newCards = markWithX <$> cards

playToLastWinner :: Game -> (Int, Card)
playToLastWinner (x:xs, cards, winners) = 
    if newWinnersL == cardsL
    then (x, lastWinner)
    else playToLastWinner (xs, newCards, newWinners)
  where
    markWithX = mark x
    newCards = markWithX <$> cards
    newWinners = filter checkCard newCards
    newWinnersL = length newWinners
    cardsL = length cards
    isExistingWinner card = any (\c -> i c == i card) winners
    lastWinner = head $ filter (not . isExistingWinner) newWinners

score :: (Int, Card) -> Int
score (lastCalled, card) = lastCalled * sum unmarked
  where
    unmarked = [ x | (x,y) <- Map.toList $ numbers card
                   , not $ checkCell card y ]

main :: IO ()
main = do
  input <- parseFromDay 4 pBingo
  putStrLn $ "Part 1: " ++ show (score $ playToFirstWinner input)
  putStrLn $ "Part 2: " ++ show (score $ playToLastWinner input)