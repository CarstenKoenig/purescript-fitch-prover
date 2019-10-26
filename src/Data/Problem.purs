module Data.Problem 
  ( Problem
  , Problems
  , ProblemNumber
  , problems
  , getProblem
  , toUnfoldable
  ) where

import Prelude

import Data.Either (fromRight)
import Data.Expressions (Expr, tryParse)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Partial.Unsafe (unsafePartial)

newtype Problems = Problems (Map ProblemNumber Problem)

derive instance newtypeProblems :: Newtype Problems _

toUnfoldable :: forall f. Unfoldable f => Problems -> f (Tuple ProblemNumber Problem)
toUnfoldable (Problems ps) = Map.toUnfoldable ps

type Problem =
  { name :: String
  , goal :: Expr
  , premisses :: Array Expr
  }

newtype ProblemNumber = ProblemNumber Int

derive instance newtypeProblemNumber :: Newtype ProblemNumber _
derive instance eqProblemNumber :: Eq ProblemNumber
derive instance ordProblemNumber :: Ord ProblemNumber
derive instance genericProblemNumber :: Generic ProblemNumber _
instance showProblemNumber :: Show ProblemNumber where
  show = genericShow

getProblem :: Problems -> ProblemNumber -> Maybe Problem
getProblem (Problems ps) index = 
  Map.lookup index ps

problems :: Problems
problems = Problems $ 
  Map.fromFoldable $
  mapWithIndex (\i v -> Tuple (ProblemNumber $ i + 1) v) $
  [ problem1
  , problem2
  , problem3
  ]



problem1 :: Problem
problem1 =
  { name: "modus ponens"
  , goal: parse "b" 
  , premisses: [parse "a", parse "a => b"]
  }

problem2 :: Problem
problem2 =
  { name: "law of excluded middle" 
  , goal: parse "a | ~a" 
  , premisses: []
  }

problem3 :: Problem
problem3 =
  { name: "contraposition"
  , goal: parse "~b => ~a" 
  , premisses: [parse "a => b"]
  }

parse :: String -> Expr
parse text = unsafePartial $
  fromRight $ tryParse text