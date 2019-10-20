module Rules
  ( RuleRecipe (..)
  , RuleInstance
  , getLabelText
  , getRuleInstance
  , filterScope
  , isCompleted
  , isValidExpr
  , allowNewExprs
  ) where

import Prelude

import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Expressions (Expr)
import Scope (Scope)
import Scope as Scope


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