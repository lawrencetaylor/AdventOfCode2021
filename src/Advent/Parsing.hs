module Advent.Parsing
  ( Parser
  , parseLinesFromDay
  , testParser
  , pInt
  , pString
  , pDigit
  , pChar
  , (.>>.)
  , (.>>)
  , (>>.)
  ) where

import           Text.Megaparsec            (ParseErrorBundle, Parsec,
                                             runParser, runParserT, many)
import           Text.Megaparsec.Char       (string, digitChar, char, space)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec as M
import qualified Data.Char as C

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

testParser :: (Show a) => Parser a -> String -> IO ()
testParser p input = do
  let test = parseString p input
  putStrLn $ show test

-- Parsing helpers
pInt :: Parser Int
pInt = L.decimal

pString :: String -> Parser String
pString = string

pDigit :: Parser Int
pDigit = C.digitToInt <$> digitChar

pChar :: Char -> Parser Char
pChar = char

(.>>.) :: Parser a -> Parser b -> Parser (a, b)
(.>>.) pA  pB = do
  a <- pA
  b <- pB
  return (a, b)

(.>>) :: Parser a -> Parser b -> Parser a
(.>>) a b = fst <$> a .>>. b

(>>.) :: Parser a -> Parser b -> Parser b
(>>.) a b = snd <$> a .>>. b