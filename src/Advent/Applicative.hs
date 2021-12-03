module Advent.Applicative(
  choice
) where

import Control.Applicative

choice :: Alternative f => [f a]-> f a
choice = foldl1 (<|>)