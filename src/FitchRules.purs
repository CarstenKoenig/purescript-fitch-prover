module FitchRules
  ( RuleInstance (..)
  , notIntroduction, notElimination
  , implicationElimination
  , andIntroduction, andElimination
  , orIntroduction, orElimination
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Expressions (Expr(..))

type RuleInstance =
  { premisses :: Array Expr
  , conclusions :: Array Expr
  }

notIntroduction :: Expr -> Expr -> Maybe RuleInstance
notIntroduction p1@(ImplExpr a b) p2@(ImplExpr a' b')
  | a == a' && (b == NegExpr b' || b' == NegExpr b) = Just
    { premisses: [p1, p2]
    , conclusions: [NegExpr a]
    }
notIntroduction _ _ = Nothing

notElimination :: Expr -> Maybe RuleInstance
notElimination p@(NegExpr (NegExpr a)) = Just
  { premisses: [p]
  , conclusions: [a]
  }
notElimination _ = Nothing

implicationElimination :: Expr -> Expr -> Maybe RuleInstance
implicationElimination p@(ImplExpr a b) a' | a == a' = Just
  { premisses: [p, a]
  , conclusions: [b]
  }
implicationElimination a' p@(ImplExpr a b) | a == a' = Just
  { premisses: [p, a]
  , conclusions: [b]
  }
implicationElimination _ _ = Nothing

andIntroduction :: Expr -> Expr -> RuleInstance
andIntroduction a b =
  { premisses: [a,b]
  , conclusions: [ AndExpr a b, AndExpr b a ]
  }

andElimination :: Expr -> Maybe RuleInstance
andElimination p@(AndExpr a b) = Just
  { premisses: [p]
  , conclusions: [a,b]
  }
andElimination _ = Nothing

orIntroduction :: Expr -> Expr -> RuleInstance
orIntroduction a b =
  { premisses: [a]
  , conclusions: [ OrExpr a b, OrExpr b a ]
  }

orElimination :: Expr -> Expr -> Expr -> Maybe RuleInstance
orElimination p1@(OrExpr a b) p2@(ImplExpr a' c) p3@(ImplExpr b' c')
  | a == a' && b == b' && c == c' = Just
    { premisses: [p1,p2,p3]
    , conclusions: [c]
    }
orElimination _ _ _ = Nothing