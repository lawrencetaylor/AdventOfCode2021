module Day12 where
import           Advent.Graph
import           Advent.Parsing
import           Data.Char      (isLower, isUpper)
import qualified Data.List      as List
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Set       (Set)
import qualified Data.Set       as Set

type Node = String
type Graph = Map Node [Node]
type NodeVisits = Map Node Int
type CanVisit = NodeVisits -> Node -> Bool

pGraph :: Parser (String,String)
pGraph = pMany pLetter .>> pChar '-' .>>. pMany pLetter

toGraph :: [(String, String)] -> Graph
toGraph = foldr (\(a, b) m -> Map.insertWith (++) a [b] m) Map.empty

canVisitSmallNodesOnce :: CanVisit
canVisitSmallNodesOnce visits node = not $ Map.member node visits

canVisitASmallNodeTwice :: CanVisit
canVisitASmallNodeTwice visits node =
  or  [ not $ Map.member node visits     -- not visited
      , all (== 1) $ Map.elems visits]  -- only visited logged nodes once

getPaths :: String -> String -> Graph -> Map String Int -> CanVisit -> [[String]]
getPaths begin end graph visitCount canVisit
    | begin == end    = [[]]
    | otherwise = [c:path | Map.member begin graph, c <- graph Map.! begin
                              , canVisit visitCount c
                              , c /= "start"
                              , path <- getPaths c end graph (v' c) canVisit]
        where v' c =
                if all isUpper c then visitCount else
                let hitCount = Map.findWithDefault 0 c visitCount
                in Map.insert c (hitCount + 1) visitCount

main :: IO ()
main = do
  input <- parseLinesFromDay 12 pGraph
  let input' = (\(x,y) -> (y,x)) <$> input -- log reverse dependencies
  let graph = toGraph $ input ++ input'    -- create graph
  let paths f = length $ getPaths "start" "end" graph Map.empty f
  putStrLn $ "Part One: " ++ show (paths canVisitSmallNodesOnce)
  putStrLn $ "Part Two: " ++ show (paths canVisitASmallNodeTwice)
