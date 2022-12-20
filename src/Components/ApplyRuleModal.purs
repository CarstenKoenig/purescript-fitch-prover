module Components.ApplyRuleModal
  ( Message(..)
  , Slot
  , Input
  , component
  ) where

import Prelude

import Components.NewExprButton as NewBtn
import Data.Array as Array
import Data.Const (Const)
import Data.Expressions (Expr)
import Data.Maybe (Maybe(..), maybe)
import Data.Rules (RuleInstance, RuleRecipe, Rule)
import Data.Rules as Rules
import Data.Scope (Scope)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import MathJaxRenderer as MathJax
import Type.Prelude (Proxy(..))

type Slot = H.Slot (Const Unit) Message

data Message
  = NewRule
      { ruleInstance :: RuleInstance
      , newFact :: Expr
      }
  | Canceled

type Input =
  { scope :: Scope
  , rule :: Rule
  }

data Action
  = Close
  | UpdateInput Input
  | ApplyExprToCurrentStep Expr
  | SelectResult { ruleInstance :: RuleInstance, newFact :: Expr }
  | HandleNewExprButton NewBtn.Message
  | Initialize

type State =
  { isActive :: Boolean
  , rule :: Rule
  , scope :: Scope
  , currentStep :: RuleRecipe
  }

type ChildSlots =
  ( newExprButton :: NewBtn.Slot Unit
  )

_newExpr :: Proxy "newExprButton"
_newExpr = Proxy

component :: forall q m. MonadEffect m => H.Component q Input Message m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< UpdateInput
        , initialize = Just Initialize
        }
    }
  where

  initialState :: Input -> State
  initialState input =
    { isActive: true
    , rule: input.rule
    , scope: input.scope
    , currentStep: Rules.autoStep input.scope input.rule.ruleRecipe
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div
      [ HP.class_ $ ClassName $ if state.isActive then "modal is-active is-clipped" else "modal" ]
      [ HH.div [ HP.class_ $ ClassName "modal-background" ] []
      , HH.div
          [ HP.class_ $ ClassName "modal-content" ]
          [ HH.section
              [ HP.class_ (ClassName "section") ]
              [ HH.div
                  [ HP.class_ (ClassName "box") ]
                  [ HH.h1 [ HP.class_ (ClassName "title") ] [ HH.text state.rule.ruleName ]
                  , HH.h2 [ HP.class_ (ClassName "subtitle") ] [ HH.text $ Rules.getLabelText state.currentStep ]
                  , maybe (HH.text "") showConclusions $ Rules.getRuleInstance state.currentStep
                  ]
              , showFacts $ Rules.filterScope state.currentStep state.scope
              , if Rules.allowNewExprs state.currentStep then HH.div
                  [ HP.class_ (ClassName "box") ]
                  [ HH.h2 [ HP.class_ (ClassName "subtitle") ] [ HH.text "you might add additional facts" ]
                  , HH.slot _newExpr unit NewBtn.component unit (HandleNewExprButton)
                  ]
                else
                  HH.text ""
              ]
          ]
      , HH.button
          [ HP.class_ (ClassName "modal-close is-large")
          , HP.attr (AttrName "aria-label") "close"
          , HE.onClick (\_ -> Close)
          ]
          []
      ]

  handleAction :: Action -> H.HalogenM State Action ChildSlots Message m Unit
  handleAction action = do
    case action of
      Close -> do
        H.modify_ (_ { isActive = false })
        H.raise Canceled
      UpdateInput input -> do
        H.modify_
          ( _
              { scope = input.scope
              , rule = input.rule
              , currentStep = input.rule.ruleRecipe
              }
          )
      ApplyExprToCurrentStep expr -> do
        step <- H.gets _.currentStep
        case step of
          Rules.Step s -> do
            st <- H.modify (\st -> st { currentStep = Rules.autoStep st.scope (s.stepNext expr) })
            autoComplete st
          _ ->
            pure unit
      Initialize -> do
        st <- H.get
        autoComplete st
      SelectResult result -> do
        H.modify_ (_ { isActive = false })
        H.raise (NewRule result)
      HandleNewExprButton (NewBtn.NewExpr expr) ->
        handleAction (ApplyExprToCurrentStep expr)
    liftEffect MathJax.typeSetPage
    where
    autoComplete st =
      case Rules.getRuleInstance st.currentStep of
        Just ruleInst ->
          case ruleInst.conclusions of
            [ single ] -> do
              H.raise (NewRule { newFact: single, ruleInstance: ruleInst })
              H.modify_ (_ { isActive = false })
            _ -> pure unit
        _ -> pure unit

  showFacts facts | Array.null facts = HH.text ""
  showFacts facts =
    HH.div
      [ HP.class_ (ClassName "box") ]
      [ HH.h1 [ HP.class_ (ClassName "subtitle") ] [ HH.text "facts in scope" ]
      , HH.div
          [ HP.class_ (ClassName "buttons is-marginless") ]
          (map showFact facts)
      ]
  showFact expr = HH.button
    [ HP.class_ (ClassName "button")
    , HE.onClick (\_ -> ApplyExprToCurrentStep expr)
    ]
    [ MathJax.showMathJax expr ]

  showConclusions ruleInst =
    if Array.null ruleInst.conclusions then HH.text ""
    else HH.div
      [ HP.class_ (ClassName "box") ]
      [ HH.h2 [ HP.class_ (ClassName "subtitle") ] [ HH.text "choose result" ]
      , HH.div
          [ HP.class_ (ClassName "buttons is-marginless") ]
          (map (showConclusion ruleInst) ruleInst.conclusions)
      ]
  showConclusion ruleInst expr = HH.button
    [ HP.class_ (ClassName "button is-success")
    , HE.onClick (\_ -> SelectResult { ruleInstance: ruleInst, newFact: expr })
    ]
    [ MathJax.showMathJax expr ]