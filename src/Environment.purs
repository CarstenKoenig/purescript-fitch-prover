module Environment 
  ( Environment
  , AssumptionStack
  , runWith
  , currentScope, scopeLevel, inScope
  , assume, introduceImplication
  , tryApply1, tryApply2, tryApply3
  ) where

import Prelude

import Control.Monad.State (State, evalState, get, gets, modify, put)
import Data.Foldable (for_)
import Expressions (Expr(..))
import Scope (Scope)
import Scope as Scope

type Environment a = State AssumptionStack a

----------------------------------------------------------------------
-- run Environment Computation

runWith :: forall a. Scope -> Environment a -> a
runWith initialScope computation = 
  evalState computation (NoAssumptions initialScope)

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
-- apply rules

tryApply1 :: (Expr -> Array Expr) -> Expr -> Environment Boolean
tryApply1  rule expr = do
  applicable <- inScope expr
  if not applicable
  then pure false
  else do
    let results = rule expr
    for_ results addExpr
    pure true

tryApply2 :: (Expr -> Expr -> Array Expr) -> Expr -> Expr -> Environment Boolean
tryApply2  rule exprA exprB = do
  applicable <- (&&) <$> inScope exprA <*> inScope exprB
  if not applicable
  then pure false
  else do
    let results = rule exprA exprB
    for_ results addExpr
    pure true

tryApply3 :: (Expr -> Expr -> Expr -> Array Expr) -> Expr -> Expr -> Expr -> Environment Boolean
tryApply3  rule exprA exprB exprC = do
  applicable <- (\a b c -> a && b && c) <$> inScope exprA <*> inScope exprB <*> inScope exprC
  if not applicable
  then pure false
  else do
    let results = rule exprA exprB exprC
    for_ results addExpr
    pure true

----------------------------------------------------------------------
-- Scope related functions

inScope :: Expr -> Environment Boolean
inScope expr = do
  cur <- currentScope
  pure $ Scope.inScope cur expr

currentScope :: Environment Scope
currentScope = do
  scopeStack <- get
  case scopeStack of
    NoAssumptions scope -> pure scope
    Assumed _ scope _ -> pure scope

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