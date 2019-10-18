module FitchRules
  ( notIntroduction, notElimination
  , implicationIntroduction, implicationElimination
  , andIntroduction, andElimination
  , orIntroduction, orElimination
  ) where

import Prelude

import Expressions (Expr(..))

notIntroduction :: Expr -> Expr -> Array Expr
notIntroduction (ImplExpr a b) (ImplExpr a' b')
  | a == a' && (b == NegExpr b' || b' == NegExpr b) = [NegExpr a]
notIntroduction _ _ = []

notElimination :: Expr -> Array Expr
notElimination (NegExpr (NegExpr a)) = [a]
notElimination _ = []

implicationIntroduction :: Expr -> Expr -> Array Expr
implicationIntroduction pred conc = [ ImplExpr pred conc ]

implicationElimination :: Expr -> Expr -> Array Expr
implicationElimination (ImplExpr a b) a' | a == a' = [b]
implicationElimination a' (ImplExpr a b) | a == a' = [b]
implicationElimination _ _ = []

andIntroduction :: Expr -> Expr -> Array Expr
andIntroduction a b = [ AndExpr a b, AndExpr b a ]

andElimination :: Expr -> Array Expr
andElimination (AndExpr a b) = [a,b]
andElimination _ = []

orIntroduction :: Expr -> Expr -> Array Expr
orIntroduction a b = [ OrExpr a b, OrExpr b a ]

orElimination :: Expr -> Expr -> Expr -> Array Expr
orElimination (OrExpr a b) (ImplExpr a' c) (ImplExpr b' c')
  | a == a' && b == b' && c == c' = [c]
orElimination _ _ _ = []