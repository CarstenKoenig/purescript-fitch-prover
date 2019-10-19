module Components.App (component) where

import Prelude

import Components.Button as Button
import Components.NewExprButton as NewBtn
import Components.Workspace as Ws
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Environment (AssumptionStack)
import Environment as Env
import Expressions (Expr)
import FitchRules (RuleInstance)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Scope as Scope

data HistoryItem
  = UsedRule { ruleInstance :: RuleInstance, newFacts :: Set Expr }

useRule :: RuleInstance -> State -> State
useRule ruleInst state =
  let (Tuple newKnowledge newStack) = Env.runWith state.currentStack (Env.tryApply ruleInst)
  in case newKnowledge of
    Nothing -> state
    Just facts -> 
      state { currentStack = newStack
            , history = UsedRule { ruleInstance: ruleInst, newFacts: facts } : state.history
            }

data Action
  = HandleButton Button.Message
  | HandleNewExprButton NewBtn.Message
  | HandleWs Ws.Message
  | CheckButtonState

type State =
  { toggleCount :: Int
  , buttonState :: Maybe Boolean
  , premisses :: Array Expr
  , currentStack :: AssumptionStack
  , history :: List HistoryItem
  }

type ChildSlots =
  ( button :: Button.Slot Unit
  , workspace :: Ws.Slot Unit
  , newExprButton :: NewBtn.Slot Unit
  )

_button :: SProxy "button"
_button = SProxy

_workspace :: SProxy "workspace"
_workspace = SProxy

_newExpr :: SProxy "newExprButton"
_newExpr = SProxy

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ =
  { toggleCount: 0
  , buttonState: Nothing
  , premisses: []
  , currentStack:  Env.NoAssumptions Scope.empty
  , history: List.Nil
  }

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.slot _button unit Button.component unit (Just <<< HandleButton)
    , HH.slot _newExpr unit NewBtn.component unit (Just <<< HandleNewExprButton)
    , HH.slot _workspace unit Ws.component unit (Just <<< HandleWs)
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

handleAction ::forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  HandleButton (Button.Toggled _) -> do
    H.modify_ (\st -> st { toggleCount = st.toggleCount + 1 })
  HandleNewExprButton (NewBtn.NewExpr _) ->
    H.modify_ (\st -> st { toggleCount = 10 })
  HandleWs (Ws.Toggled _) -> do
    H.modify_ (\st -> st { toggleCount = st.toggleCount - 1 })
  CheckButtonState -> do
    buttonState <- H.query _button unit $ H.request Button.IsOn
    H.modify_ (_ { buttonState = buttonState })