module Data.Problem 
  ( Problem
  , getProblem
  , problems
  , problem1
  , problem2
  , problem3
  ) where

import Prelude

import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Data.Expressions (Expr, tryParse)
import Partial.Unsafe (unsafePartial)

type Problem =
  { name :: String
  , number :: Int
  , goal :: Expr
  , premisses :: Array Expr
  }

getProblem :: Int -> Maybe Problem
getProblem = case _ of
  1 -> Just problem1
  2 -> Just problem2
  3 -> Just problem3
  _ -> Nothing

problems :: Array Problem
problems =
  [ problem1
  , problem2
  , problem3
  ]

problem1 :: Problem
problem1 =
  { name: "modus ponens"
  , number: 1
  , goal: parse "b" 
  , premisses: [parse "a", parse "a => b"]
  }

problem2 :: Problem
problem2 =
  { name: "law of excluded middle" 
  , number: 2
  , goal: parse "a | ~a" 
  , premisses: []
  }

problem3 :: Problem
problem3 =
  { name: "contraposition"
  , number: 3
  , goal: parse "~b => ~a" 
  , premisses: [parse "a => b"]
  }

parse :: String -> Expr
parse text = unsafePartial $
  fromRight $ tryParse text