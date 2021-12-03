module Day02 where

import           Advent.Applicative
import           Advent.Parsing
import           Control.Applicative (Applicative (..), (<|>))
import           Data.Functor        (($>))

data Instruction =
  Forward Int | Down Int | Up Int
  deriving (Show, Eq)

pInstruction :: Parser Instruction
pInstruction = choice
  [ Up <$> pString "up " >>. pInt
  , Down <$> pString "down " >>. pInt
  , Forward <$> pString "forward " >>. pInt
  ]

type Position = (Int, Int)

move1 :: Position -> Instruction -> Position
move1 (x,y) (Forward n) = (x + n, y)
move1 (x,y) (Up n)      = (x, y - n)
move1 (x,y) (Down n)    = (x, y + n)

type State = (Position, Int)

move2 :: State -> Instruction -> State
move2 (p, a) (Down n)       = (p, a + n)
move2 (p, a) (Up n)         = (p, a - n)
move2 ((x,y), a)(Forward n) = ((x+n, y + (a * n)), a)

main :: IO ()
main = do
  input <- parseLinesFromDay 2 pInstruction
  let (x1,y1) = foldl move1 (0, 0) input
  let ((x2,y2),_) = foldl move2 ((0,0), 0) input
  putStrLn $ "Part One: " ++ show (x1*y1)
  putStrLn $ "Part Two: " ++ show (x2*y2)
