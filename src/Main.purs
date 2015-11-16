module Main where

import Control.Monad.Eff.Console
import Data.List.Lazy
import Data.Traversable
import GameOfLife
import Prelude

main = traverse
  (traverse log <<< Data.List.(:) "--------------" <<< prettyWorld)
  (take 5 $ iterate evolve threeInARow)
