module Advent.Parsing
  ( parseLinesFromDay
  , pInt
  ) where

import           Text.Megaparsec            (ParseErrorBundle, Parsec,
                                             runParser, runParserT)
import           Text.Megaparsec.Char       ()
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec () String

type ParseError = ParseErrorBundle String ()

-- Processing file input

padDay :: Int -> String
padDay d =
  let s = show d
   in replicate (2 - length s) '0' ++ s

fileNameForDay :: Int -> String
fileNameForDay day = "input/day" ++ padDay day ++ ".txt"

readFileForDay :: Int -> IO String
readFileForDay day = readFile (fileNameForDay day)

readLinesForDay :: Int -> IO [String]
readLinesForDay day = fmap lines (readFileForDay day)

-- Paring file input

parseString :: Parser a -> String -> Either ParseError a
parseString p = runParser p ""

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p file = parseString p <$> readFile file

parseLinesFromDay :: Int -> Parser a -> IO [a]
parseLinesFromDay file p = do 
  (Right parsedResult) <- sequence <$> fmap (fmap (parseString p)) (readLinesForDay file)
  return parsedResult

-- Parsing helpers
pInt :: Parser Int
pInt = L.decimal