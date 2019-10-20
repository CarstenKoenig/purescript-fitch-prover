module Components.App (component) where

import Prelude

import Components.ApplyRuleModal as RuleDlg
import Data.Array as Array
import Data.Either (either)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Environment (AssumptionStack)
import Environment as Env
import Expressions (Expr, tryParse)
import FitchRules as Fitch
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rules (Rule, RuleInstance)
import Rules as Rules
import Scope (Scope)
import Scope as Scope

data HistoryItem
  = UsedRule { ruleInstance :: RuleInstance, newFact :: Expr }
  | AddedPremisse { premisse :: Expr }

useRule :: { ruleInstance :: RuleInstance, newFact :: Expr } -> State -> State
useRule r state =
  let (Tuple _ newStack) = Env.runWith state.currentStack (Env.tryApply r.ruleInstance)
  in state 
    { currentStack = newStack
    , history = UsedRule r : state.history
    }

addPremisse :: Expr -> State -> State
addPremisse prem state =
  let (Tuple _ newStack) = Env.runWith state.currentStack (Env.addExpr prem)
  in state 
    { currentStack = newStack
    , history = AddedPremisse { premisse: prem } : state.history
    }

showHistory :: forall w i. Array HistoryItem -> HTML w i
showHistory items =
  HH.div
    [ HP.class_ (ClassName "box") ] 
    [ HH.h1 [ HP.class_ (ClassName "subtitle") ] [ HH.text "history" ]
    , HH.ol_
      (map showItem items)
    ]
  where
  showItem (AddedPremisse p) = 
    HH.li_ [ HH.span_ [ HH.text "premisse: ", HH.strong_ [ HH.text $ show p.premisse ] ] ]
  showItem (UsedRule r) =
    HH.li_ 
      [ HH.span_ 
        [ HH.strong_ [ HH.text $ show r.newFact ] 
        , HH.text " by rule "
        , HH.em_ [ HH.text r.ruleInstance.description ] 
        ]
      ] 

data Action
  = ShowRuleModal Rule
  | HandleRuleModal RuleDlg.Message

type State =
  { showRuleModal :: Maybe Rule
  , premisses :: Array Expr
  , currentStack :: AssumptionStack
  , history :: List HistoryItem
  }

type ChildSlots =
  ( newRuleModal :: RuleDlg.Slot Unit
  )

_newRuleModal :: SProxy "newRuleModal"
_newRuleModal = SProxy

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ =
  either (const identity) addPremisse (tryParse "a | b") $
  either (const identity) addPremisse (tryParse "~(~a)") $
  { showRuleModal: Nothing
  , premisses: []
  , currentStack:  Env.NoAssumptions Scope.empty
  , history: List.Nil
  }

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
render state = HH.div_
  [ case state.showRuleModal of
      Just rule -> HH.slot _newRuleModal unit 
        RuleDlg.component 
          { scope: Env.scopeOf state.currentStack 
          , rule
          } 
        (Just <<< HandleRuleModal)
      Nothing -> HH.text "" 
  , HH.div_
    [ showHistory (List.toUnfoldable $ List.reverse state.history) 
    , showRuleButtons (Env.scopeOf state.currentStack) Fitch.rules
    ]
  ]

handleAction ::forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  ShowRuleModal rule ->
    H.modify_ (\st -> st { showRuleModal = Just rule })
  HandleRuleModal RuleDlg.Canceled ->
    H.modify_ (\st -> st { showRuleModal = Nothing })
  HandleRuleModal (RuleDlg.NewRule r) ->
    H.modify_ (\st -> useRule r $ st { showRuleModal = Nothing })

showRuleButtons :: forall w. Scope -> Array Rule -> HTML w Action
showRuleButtons _ rules | Array.null rules = HH.text ""
showRuleButtons scope rules =
  HH.div
    [ HP.class_ (ClassName "box") ] 
    [ HH.h1 [ HP.class_ (ClassName "subtitle") ] [ HH.text "rules" ]
    , HH.div
      [ HP.class_ (ClassName "buttons is-marginless") ]
      (map showRuleButton rules)
    ]
  where
  showRuleButton rule = HH.button 
    [ HP.class_ (ClassName "button") 
    , HP.disabled (not $ Rules.isUsableWith rule.ruleRecipe scope)
    , HE.onClick (\_ -> Just (ShowRuleModal rule))
    ] 
    [ HH.text rule.ruleName ]