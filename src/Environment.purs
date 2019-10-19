module Environment 
  ( Environment
  , AssumptionStack (..)
  , runWith
  , currentScope, scopeLevel, inScope
  , assume, introduceImplication
  , tryApply
  , scopeOf
  , addExpr
  ) where

import Prelude

import Control.Monad.State (State, evalState, get, gets, modify, put, runState)
import Data.Foldable (foldM, for_)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Tuple (Tuple)
import Expressions (Expr(..))
import FitchRules (RuleInstance)
import Scope (Scope)
import Scope as Scope

type Environment a = State AssumptionStack a

----------------------------------------------------------------------
-- run Environment Computation

evalWith :: forall a. Scope -> Environment a -> a
evalWith initialScope computation = 
  evalState computation (NoAssumptions initialScope)

runWith :: forall a. AssumptionStack -> Environment a -> Tuple a AssumptionStack
runWith stack computation = 
  runState computation stack

----------------------------------------------------------------------
-- introduce implications

assume :: Expr -> Environment Unit
assume expr = do
  newScope <- (flip Scope.include expr) <$> currentScope
  void $ modify (Assumed expr newScope)

introduceImplication :: Expr -> Environment Boolean
introduceImplication conclusion = do
  curStack <- get
  case curStack of
    (Assumed expr subScope mainStack) | Scope.inScope subScope conclusion -> do
      modifyScope (flip Scope.include $ ImplExpr expr conclusion) 
      pure true
    _ -> pure false

----------------------------------------------------------------------
-- tries to apply rule-results

tryApply :: RuleInstance -> Environment (Maybe (Set Expr))
tryApply ruleInst = do
  applicable <- foldM (\ok p -> (ok && _) <$> inScope p) true ruleInst.premisses
  newFacts <- Scope.newKnowledge ruleInst.conclusions <$> currentScope
  if not applicable
  then pure Nothing
  else do
    for_ newFacts addExpr
    pure (Just newFacts)

----------------------------------------------------------------------
-- Scope related functions

inScope :: Expr -> Environment Boolean
inScope expr = do
  cur <- currentScope
  pure $ Scope.inScope cur expr

currentScope :: Environment Scope
currentScope = gets scopeOf

modifyScope :: (Scope -> Scope) -> Environment Unit
modifyScope mod = do
  scopeStack <- get
  case scopeStack of
    NoAssumptions scope -> put (NoAssumptions $ mod scope)
    Assumed expr scope rest -> put (Assumed expr (mod scope) rest)

scopeLevel :: Environment Int
scopeLevel = gets stackDepth

addExpr :: Expr -> Environment Unit
addExpr expr = modifyScope (flip Scope.include expr)

----------------------------------------------------------------------
-- Assumption Stack - tracks scopes and assumptions for implications

data AssumptionStack
  = NoAssumptions Scope
  | Assumed Expr Scope AssumptionStack

stackDepth :: AssumptionStack -> Int
stackDepth (NoAssumptions _) = 0
stackDepth (Assumed _ _ stack) = 1 + stackDepth stack

scopeOf :: AssumptionStack -> Scope
scopeOf stack =
  case stack of
    NoAssumptions scope -> scope
    Assumed _ scope _ -> scope