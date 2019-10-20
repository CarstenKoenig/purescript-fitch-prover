module Components.ApplyRuleModal
  ( Message (..)
  , Slot
  , Input
  , component
  ) where

import Prelude

import Components.NewExprButton as NewBtn
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Expressions (Expr)
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rules (RuleInstance, RuleRecipe, Rule)
import Rules as Rule
import Rules as Rules
import Scope (Scope)
import Scope as Scope

type Slot = H.Slot (Const Unit) Message

data Message 
  = NewRule { ruleInstance :: RuleInstance
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

type State =
  { isActive :: Boolean 
  , rule :: Rule
  , scope :: Scope
  , currentStep :: RuleRecipe
  }

type ChildSlots =
  ( newExprButton :: NewBtn.Slot Unit
  )

_newExpr :: SProxy "newExprButton"
_newExpr = SProxy

component :: forall q m. MonadEffect m => H.Component HH.HTML q Input Message m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      , receive = Just <<< UpdateInput
      }
    }
  where

  initialState :: Input -> State
  initialState input =
    { isActive: true 
    , rule: input.rule
    , scope: input.scope
    , currentStep: input.rule.ruleRecipe
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
            , HH.div
              [ HP.class_ (ClassName "box") ] 
              [ HH.h1 [ HP.class_ (ClassName "subtitle") ] [ HH.text "facts in scope" ]
              , showFacts $ Rule.filterScope state.currentStep state.scope 
              ]
            , if Rules.allowNewExprs state.currentStep 
              then HH.div
                [ HP.class_ (ClassName "box") ] 
                [ HH.h2 [ HP.class_ (ClassName "subtitle") ] [ HH.text "you might add additional facts" ]
                , HH.slot _newExpr unit NewBtn.component unit (Just <<< HandleNewExprButton)
                ]
              else
                HH.text ""
            ]
          ]
        , HH.button
          [ HP.class_ (ClassName "modal-close is-large")
          , HP.attr (AttrName "aria-label") "close"
          , HE.onClick (\_ -> Just Close)
          ]
          []
        ]

  handleAction :: Action -> H.HalogenM State Action ChildSlots Message m Unit
  handleAction = case _ of
    Close -> do
      H.modify_ (_ { isActive = false })
      H.raise Canceled
    UpdateInput input -> do
      H.modify_ (_ { scope = input.scope
                   , rule = input.rule
                   , currentStep = input.rule.ruleRecipe
                   })
    ApplyExprToCurrentStep expr -> do
      step <- H.gets _.currentStep
      case step of
        Rules.Step s -> H.modify_ (_ { currentStep = s.stepNext expr })
        _ -> pure unit
    SelectResult result -> do
      H.modify_ (_ { isActive = false })
      H.raise (NewRule result)
    HandleNewExprButton (NewBtn.NewExpr expr) ->
      H.modify_ (\st -> st { scope = Scope.include st.scope expr })

  showFacts facts = HH.div
    [ HP.class_ (ClassName "buttons is-marginless") ]
    (map showFact facts)
  showFact expr = HH.button 
    [ HP.class_ (ClassName "button") 
    , HE.onClick (\_ -> Just (ApplyExprToCurrentStep expr))
    ] 
    [ HH.text $ show expr ]

  showConclusions ruleInst = HH.div
    [ HP.class_ (ClassName "buttons is-marginless") ]
    (map (showConclusion ruleInst) ruleInst.conclusions)
  showConclusion ruleInst expr = HH.button 
    [ HP.class_ (ClassName "button") 
    , HE.onClick (\_ -> Just $ SelectResult { ruleInstance: ruleInst, newFact: expr })
    ] 
    [ HH.text $ show expr ]