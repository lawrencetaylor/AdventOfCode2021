module Advent.Graph where
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Set as Set

newtype Queue a =
  Queue [a]
  deriving (Show, Eq)

push :: a -> Queue a -> Queue a
push a (Queue q) = Queue (q ++ [a])

pushAll :: [a] -> Queue a -> Queue a
pushAll la q = foldr push q (reverse la)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue q) =
  case q of
    []     -> Nothing
    (x:xs) -> Just (x, Queue xs)

isEmpty :: Queue a -> Bool
isEmpty (Queue []) = True
isEmpty (Queue _) = False

instance (Ord a) => Ord (Queue a )where
  (Queue a) <= (Queue b) = a <= b

instance Foldable Queue where
  foldMap f (Queue q) = foldMap f (reverse q)

{-|
Conducts a breadth-first search on a graph
-}
bfs :: (Ord b) =>
      (a -> [a])   -- ^ Defines all the NEXT values from a given starting point
      -> (a -> b)  -- ^ Defines the value we associate to all nodes of the graph
      -> a         -- ^ Defines the starting node
      -> [a]       -- ^ Defines all nodes in the graph
bfs next rep a   =
  Maybe.catMaybes $
  List.unfoldr (bfsUnfold next) (Set.empty, Queue [a])
  where
    bfsUnfold next (seen, q) =
      case pop q of
        Nothing -> Nothing
        Just (p, qs) ->
          if rep p `Set.member` seen then
            Just (Nothing, (seen, qs))
          else
              Just (Just p, (Set.insert (rep p) seen, pushAll (next p) qs))