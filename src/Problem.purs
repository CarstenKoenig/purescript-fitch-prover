module Problem 
  ( Problem
  , problem1
  , problem2
  ) where

import Prelude

import Data.Either (fromRight)
import Expressions (Expr, tryParse)
import Partial.Unsafe (unsafePartial)

type Problem =
  { goal :: Expr
  , premisses :: Array Expr
  }

problem1 :: Problem
problem1 =
  { goal: parse "b" 
  , premisses: [parse "a", parse "a => b"]
  }

problem2 :: Problem
problem2 =
  { goal: parse "a | ~a" 
  , premisses: []
  }

parse :: String -> Expr
parse text = unsafePartial $
  fromRight $ tryParse text