module Data.Rules
  ( Rule
  , RuleRecipe (..)
  , RuleInstance
  , getLabelText
  , getRuleInstance
  , filterScope
  , isCompleted
  , conclusions
  , isValidExpr
  , allowNewExprs
  , isUsableWith
  , autoStep
  ) where

import Prelude

import Data.Array (any, filter, mapMaybe)
import Data.Expressions (Expr)
import Data.Maybe (Maybe(..))
import Data.Scope (Scope)
import Data.Scope as Scope
import Description (Description)

type Rule =
  { ruleName :: String
  , ruleRecipe :: RuleRecipe
  }

data RuleRecipe
  = Step 
    { stepLabel :: String
    , stepIsValidExpr :: Expr -> Boolean 
    , stepAllowNewExprs :: Boolean
    , stepNext :: Expr -> RuleRecipe
    }
  | Failed
  | Succeeded RuleInstance

type RuleInstance =
  { description :: Description
  , premisses :: Array Expr
  , conclusions :: Array Expr
  }


getLabelText :: RuleRecipe -> String
getLabelText (Step s) = s.stepLabel
getLabelText Failed = "sorry that did not work out"
getLabelText (Succeeded ruleInst) = show ruleInst.description

getRuleInstance :: RuleRecipe -> Maybe RuleInstance
getRuleInstance (Step _) = Nothing
getRuleInstance Failed = Nothing
getRuleInstance (Succeeded ruleInst) = Just ruleInst

filterScope :: RuleRecipe -> Scope -> Array Expr
filterScope (Step s) = filter s.stepIsValidExpr <<< Scope.toArray 
filterScope Failed = const []
filterScope (Succeeded _) = const []

isCompleted :: RuleRecipe -> Boolean
isCompleted (Step _) = false
isCompleted Failed = true
isCompleted (Succeeded _) = true

conclusions :: RuleRecipe -> Array Expr
conclusions (Step _) = []
conclusions Failed = []
conclusions (Succeeded ruleInst) = ruleInst.conclusions

isValidExpr :: RuleRecipe -> Expr -> Boolean
isValidExpr (Step s) = s.stepIsValidExpr
isValidExpr Failed = const false
isValidExpr (Succeeded _) = const false

allowNewExprs :: RuleRecipe -> Boolean
allowNewExprs (Step s) = s.stepAllowNewExprs
allowNewExprs Failed = false
allowNewExprs (Succeeded _) = false

-- | this function tries to check if the given rule-recipe
-- | can possibly be used in the context given by Scope
-- | please note that it'll not try to find new facts
-- | in case a step allows creating those it'll return true at
-- | this point
isUsableWith :: RuleRecipe -> Scope -> Boolean
isUsableWith Failed _ = false
isUsableWith (Succeeded _) _ = true
isUsableWith (Step s) _ | s.stepAllowNewExprs = true
isUsableWith (Step s) scope = any tryFindAssignments $ Scope.toArray scope
  where
  tryFindAssignments expr | s.stepIsValidExpr expr =
    isUsableWith (s.stepNext expr) scope
  tryFindAssignments _ = false

-- | continues to use Facts in Scope as there are only one fitting
autoStep :: Scope -> RuleRecipe -> RuleRecipe
autoStep _ Failed = Failed
autoStep _ st@(Succeeded _) = st
autoStep _ st@(Step s) | s.stepAllowNewExprs = st
autoStep scope st@(Step s) =
  let fittingFacts = mapMaybe tryStep $ Scope.toArray scope
  in case fittingFacts of
    [single] -> autoStep scope single
    _ -> st
  where
  tryStep expr | s.stepIsValidExpr expr =
    let nextStep = s.stepNext expr
    in if isUsableWith nextStep scope then Just nextStep else Nothing
  tryStep _ = Nothing