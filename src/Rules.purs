module Rules
  ( Rule
  , RuleRecipe (..)
  , RuleInstance
  , getLabelText
  , getRuleInstance
  , filterScope
  , isCompleted
  , isValidExpr
  , allowNewExprs
  , isUsableWith
  ) where

import Prelude

import Data.Array (any, filter)
import Data.Maybe (Maybe(..))
import Expressions (Expr)
import Scope (Scope)
import Scope as Scope

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
  { description :: String
  , premisses :: Array Expr
  , conclusions :: Array Expr
  }


getLabelText :: RuleRecipe -> String
getLabelText (Step s) = s.stepLabel
getLabelText Failed = "sorry that did not work out"
getLabelText (Succeeded ruleInst) = ruleInst.description

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
