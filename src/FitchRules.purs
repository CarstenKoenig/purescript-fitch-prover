module FitchRules
  ( notIntroduction, notElimination
  , implicationElimination
  , andIntroduction, andElimination
  , orIntroduction, orElimination
  ) where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Expressions (Expr(..))
import Rules (RuleInstance, RuleRecipe(..), Rule)

notIntroduction :: Expr -> Expr -> Maybe RuleInstance
notIntroduction p1@(ImplExpr a b) p2@(ImplExpr a' b')
  | a == a' && (b == NegExpr b' || b' == NegExpr b) = Just
    { description: fold ["NOT-introduction: [", show p1, "], [", show p2, "]"]
    , premisses: [p1, p2]
    , conclusions: [NegExpr a]
    }
notIntroduction _ _ = Nothing

notElimination :: Rule 
notElimination = 
    { ruleName: "NOT elimination"
    , ruleRecipe: recipe
    }
    where 
      recipe = Step
        { stepLabel: "Choose a known fact."
        , stepIsValidExpr: validDoubleNot
        , stepAllowNewExprs: false
        , stepNext: step
        }
      step p@(NegExpr (NegExpr a)) = Succeeded
        { description: fold ["NOT-elimination of ", show p]
        , premisses: [p]
        , conclusions: [a]
        }
      step _ = Failed
      validDoubleNot (NegExpr (NegExpr _)) = true
      validDoubleNot _ = false

implicationElimination :: Expr -> Expr -> Maybe RuleInstance
implicationElimination p@(ImplExpr a b) a' | a == a' = Just
  { description: fold ["=> - elimination: [", show p, "], [", show a, "]"]
  , premisses: [p, a]
  , conclusions: [b]
  }
implicationElimination a' p@(ImplExpr a b) | a == a' = Just
  { description: fold ["=> - elimination: [", show p, "], [", show a, "]"]
  , premisses: [p, a]
  , conclusions: [b]
  }
implicationElimination _ _ = Nothing

andIntroduction :: Expr -> Expr -> RuleInstance
andIntroduction a b =
  { description: fold ["AND-introduction: [", show a, "], [", show b, "]"]
  , premisses: [a,b]
  , conclusions: [ AndExpr a b, AndExpr b a ]
  }

andElimination :: Expr -> Maybe RuleInstance
andElimination p@(AndExpr a b) = Just
  { description: fold ["AND-elimination: [", show p, "]"]
  , premisses: [p]
  , conclusions: [a,b]
  }
andElimination _ = Nothing

orIntroduction :: Expr -> Expr -> RuleInstance
orIntroduction a b =
  { description: fold ["OR-introduction: [", show a, "] | [", show b, "]"]
  , premisses: [a]
  , conclusions: [ OrExpr a b, OrExpr b a ]
  }

orElimination :: Expr -> Expr -> Expr -> Maybe RuleInstance
orElimination p1@(OrExpr a b) p2@(ImplExpr a' c) p3@(ImplExpr b' c')
  | a == a' && b == b' && c == c' = Just
    { description: fold [ "OR-elimination: [", show b, "], [", show c, "] with [", show a, "]"] 
    , premisses: [p1,p2,p3]
    , conclusions: [c]
    }
orElimination _ _ _ = Nothing