module Data.FitchRules
  ( notIntroduction, notElimination
  , implicationElimination
  , andIntroduction, andElimination
  , orIntroduction, orElimination
  , rules
  ) where

import Prelude

import Data.Array as Array
import Data.Expressions (Expr(..))
import Data.Rules (Rule, RuleRecipe(..))
import Description as Desc

rules :: Array Rule
rules =
  [ notIntroduction, notElimination
  , implicationElimination
  , andIntroduction, andElimination
  , orIntroduction, orElimination
  ]

notIntroduction :: Rule
notIntroduction =
  { ruleName: "NOT introduction"
  , ruleRecipe: recipe
  }
  where
  recipe = step1
  step1 = Step
    { stepLabel: "Choose an implication"
    , stepIsValidExpr: validStep1
    , stepAllowNewExprs: false
    , stepNext: step2
    }
  validStep1 (ImplExpr _ _) = true
  validStep1 _ = false
  step2 p1@(ImplExpr a b) = Step
    { stepLabel: "Choose contradiction"
    , stepIsValidExpr: validStep2 p1
    , stepAllowNewExprs: false
    , stepNext: complete p1
    }
  step2 _ = Failed
  validStep2 (ImplExpr a b) (ImplExpr a' b') =
    a == a' && b' == NegExpr b || b == NegExpr b'
  validStep2 _ _ = false
  complete p1@(ImplExpr a _) p2 | validStep2 p1 p2 = Succeeded
    { description: Desc.desc [ Desc.text "introduced NOT from ", Desc.expr p1, Desc.text " and ", Desc.expr p2]
    , premisses: [p1, p2]
    , conclusions: [NegExpr a]
    }
  complete _ _ = Failed

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
        { description: Desc.desc [ Desc.text "eliminated NOT in ", Desc.expr p]
        , premisses: [p]
        , conclusions: [a]
        }
      step _ = Failed
      validDoubleNot (NegExpr (NegExpr _)) = true
      validDoubleNot _ = false

implicationElimination :: Rule
implicationElimination =
  { ruleName: "=> elimination"
  , ruleRecipe: recipe
  }
  where
  recipe = step1
  step1 = Step
    { stepLabel: "Choose an implication"
    , stepIsValidExpr: validStep1
    , stepAllowNewExprs: false
    , stepNext: step2
    }
  validStep1 (ImplExpr _ _) = true
  validStep1 _ = false
  step2 p1@(ImplExpr a b) = Step
    { stepLabel: "choose premisse"
    , stepIsValidExpr: validStep2 p1
    , stepAllowNewExprs: false
    , stepNext: complete p1
    }
  step2 _ = Failed
  validStep2 (ImplExpr a _) a' =
    a == a'
  validStep2 _ _ = false
  complete p1@(ImplExpr a b) a' | validStep2 p1 a' = Succeeded
    { description: Desc.desc [ Desc.text "eliminated => in ", Desc.expr p1, Desc.text " with ", Desc.expr a']
    , premisses: [p1, a']
    , conclusions: [b]
    }
  complete _ _ = Failed

andIntroduction :: Rule
andIntroduction =
  { ruleName: "AND introduction"
  , ruleRecipe: recipe
  }
  where
  recipe = step1
  step1 = Step
    { stepLabel: "Choose a known fact."
    , stepIsValidExpr: const true
    , stepAllowNewExprs: false
    , stepNext: step2
    }
  step2 a = Step
    { stepLabel: "Choose a second fact."
    , stepIsValidExpr: const true
    , stepAllowNewExprs: false
    , stepNext: complete a
    }
  complete a b = Succeeded
    { description: Desc.desc [ Desc.text "introduced AND from ", Desc.expr a, Desc.text " and ", Desc.expr b]
    , premisses: [a,b]
    , conclusions: Array.nub [ AndExpr a b, AndExpr b a ]
    }

andElimination :: Rule 
andElimination = 
    { ruleName: "AND elimination"
    , ruleRecipe: recipe
    }
    where 
      recipe = Step
        { stepLabel: "Choose a known conjunction."
        , stepIsValidExpr: validAnd
        , stepAllowNewExprs: false
        , stepNext: step
        }
      step p@(AndExpr a b) = Succeeded
        { description: Desc.desc [ Desc.text "from ", Desc.expr p]
        , premisses: [p]
        , conclusions: Array.nub [a, b]
        }
      step _ = Failed
      validAnd (AndExpr _ _) = true
      validAnd _ = false

orIntroduction :: Rule
orIntroduction =
  { ruleName: "OR introduction"
  , ruleRecipe: recipe
  }
  where
  recipe = step1
  step1 = Step
    { stepLabel: "Choose a known fact."
    , stepIsValidExpr: const true
    , stepAllowNewExprs: false
    , stepNext: step2
    }
  step2 a = Step
    { stepLabel: "Choose a second."
    , stepIsValidExpr: const true
    , stepAllowNewExprs: true
    , stepNext: complete a
    }
  complete a b = Succeeded
    { description: Desc.desc [ Desc.text "introduced OR from sure ", Desc.expr a, Desc.text " and ", Desc.expr b]
    , premisses: [a]
    , conclusions: Array.nub [ OrExpr a b, OrExpr b a ]
    }

orElimination :: Rule 
orElimination = 
    { ruleName: "OR elimination"
    , ruleRecipe: recipe
    }
    where 
      recipe = Step
        { stepLabel: "Choose a known fact."
        , stepIsValidExpr: validOr
        , stepAllowNewExprs: false
        , stepNext: step1
        }
      validOr (OrExpr _ _) = true
      validOr _ = false
      step1 or@(OrExpr a _) = Step
        { stepLabel: "Choose first implication."
        , stepIsValidExpr: validImpl1 a
        , stepAllowNewExprs: false
        , stepNext: step2 or
        }
      step1 _ = Failed
      validImpl1 a (ImplExpr a' _) | a == a' = true
      validImpl1 _ _ = false
      step2 or@(OrExpr a b) i1@(ImplExpr _ c) | validImpl1 a i1 = Step
        { stepLabel: "Choose second implication."
        , stepIsValidExpr: validImpl2 b c
        , stepAllowNewExprs: false
        , stepNext: completed or i1
        }
      step2 _ _ = Failed
      validImpl2 b c (ImplExpr b' c') | b == b' && c == c' = true
      validImpl2 _ _ _ = false
      completed or@(OrExpr a b) i1@(ImplExpr _ c) i2 | validImpl2 b c i2  = Succeeded
        { description: Desc.desc [ Desc.text "eliminated OR in ", Desc.expr or, Desc.text " with ", Desc.expr i1, Desc.text " and ", Desc.expr i2]
        , premisses: [or, i1, i2]
        , conclusions: [c]
        }
      completed _ _ _ = Failed