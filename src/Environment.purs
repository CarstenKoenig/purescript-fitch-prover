module Environment 
  ( Environment
  , AssumptionStack
  , runWith
  , currentScope, scopeLevel, inScope
  , assume, introduceImplication
  ) where

import Prelude

import Control.Monad.State (State, evalState, get, gets, modify, put)
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
      put $ addToScope (ImplExpr expr conclusion) mainStack
      pure true
    _ -> pure false

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

scopeLevel :: Environment Int
scopeLevel = gets stackDepth

----------------------------------------------------------------------
-- Assumption Stack - tracks scopes and assumptions for implications

data AssumptionStack
  = NoAssumptions Scope
  | Assumed Expr Scope AssumptionStack

stackDepth :: AssumptionStack -> Int
stackDepth (NoAssumptions _) = 0
stackDepth (Assumed _ _ stack) = 1 + stackDepth stack

addToScope :: Expr -> AssumptionStack -> AssumptionStack
addToScope expr (NoAssumptions scope) = 
  NoAssumptions (Scope.include scope expr)
addToScope expr (Assumed e scope rest) = 
  Assumed e (Scope.include scope expr) rest

