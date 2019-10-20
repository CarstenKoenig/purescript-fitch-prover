module Components.App (component) where

import Prelude

import Components.ApplyRuleModal as RuleDlg
import Components.Button as Button
import Components.NewExprButton as NewBtn
import Data.Either (either)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Environment (AssumptionStack)
import Environment as Env
import Expressions (Expr, tryParse)
import FitchRules as Fitch
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Rules (RuleInstance)
import Scope as Scope

data HistoryItem
  = UsedRule { ruleInstance :: RuleInstance, newFacts :: Set Expr }
  | AddedPremisse { premisse :: Expr }

useRule :: RuleInstance -> State -> State
useRule ruleInst state =
  let (Tuple newKnowledge newStack) = Env.runWith state.currentStack (Env.tryApply ruleInst)
  in case newKnowledge of
    Nothing -> state
    Just facts -> 
      state { currentStack = newStack
            , history = UsedRule { ruleInstance: ruleInst, newFacts: facts } : state.history
            }

addPremisse :: Expr -> State -> State
addPremisse prem state =
  let (Tuple _ newStack) = Env.runWith state.currentStack (Env.addExpr prem)
  in state 
    { currentStack = newStack
    , history = AddedPremisse { premisse: prem } : state.history
    }

data Action
  = HandleButton Button.Message
  | HandleNewExprButton NewBtn.Message
  | HandleRuleModal RuleDlg.Message
  | CheckButtonState

type State =
  { toggleCount :: Int
  , buttonState :: Maybe Boolean
  , showRuleModal :: Boolean
  , premisses :: Array Expr
  , currentStack :: AssumptionStack
  , history :: List HistoryItem
  }

type ChildSlots =
  ( button :: Button.Slot Unit
  , newExprButton :: NewBtn.Slot Unit
  , newRuleModal :: RuleDlg.Slot Unit
  )

_button :: SProxy "button"
_button = SProxy

_newExpr :: SProxy "newExprButton"
_newExpr = SProxy

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
  { toggleCount: 0
  , buttonState: Nothing
  , showRuleModal: true
  , premisses: []
  , currentStack:  Env.NoAssumptions Scope.empty
  , history: List.Nil
  }

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
render state = HH.div_
  [ if state.showRuleModal
    then HH.slot _newRuleModal unit 
      RuleDlg.component 
        { scope: Env.scopeOf state.currentStack 
        , mayIntroduceAdditionalFacts: true
        , recipe: exampleRecipe
        } 
      (Just <<< HandleRuleModal)
    else HH.text "" 
  , HH.div_
    [ HH.slot _button unit Button.component unit (Just <<< HandleButton)
    , HH.slot _newExpr unit NewBtn.component unit (Just <<< HandleNewExprButton)
    , HH.p_
        [ HH.text ("Button has been toggled " <> show state.toggleCount <> " time(s)") ]
    , HH.p_
        [ HH.text
            $ "Last time I checked, the button was: "
            <> (maybe "(not checked yet)" (if _ then "on" else "off") state.buttonState)
            <> ". "
        , HH.button
            [ HE.onClick (\_ -> Just CheckButtonState) ]
            [ HH.text "Check now" ]
        ]
    ]
  ]

handleAction ::forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  HandleRuleModal RuleDlg.Canceled ->
    H.modify_ (\st -> st { showRuleModal = false })
  HandleRuleModal (RuleDlg.NewRule _) ->
    H.modify_ (\st -> st { showRuleModal = false })
  HandleButton (Button.Toggled _) -> do
    H.modify_ (\st -> st { toggleCount = st.toggleCount + 1 })
  HandleNewExprButton (NewBtn.NewExpr _) ->
    H.modify_ (\st -> st { showRuleModal = true })
  CheckButtonState -> do
    buttonState <- H.query _button unit $ H.request Button.IsOn
    H.modify_ (_ { buttonState = buttonState })

exampleRecipe :: RuleDlg.RuleRecipe
exampleRecipe =
  RuleDlg.Step
    { stepLabel: "OR-introduction know fact?"
    , step: \fact -> RuleDlg.Step
      { stepLabel: "Or-introduction additional expr"
      , step: \expr -> RuleDlg.Completed (Fitch.orIntroduction fact expr)
      }
    }