module Components.ApplyRuleModal
  ( Message (..)
  , Slot
  , Input
  , component
  ) where

import Prelude

import Components.NewExprButton as NewBtn
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import FitchRules (RuleInstance)
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Scope (Scope)
import Scope as Scope

type Slot = H.Slot (Const Unit) Message

data Message 
  = NewRule RuleInstance
  | Canceled

type Input =
  { scope :: Scope 
  , mayIntroduceAdditionalFacts :: Boolean
  }

data Action 
  = Close
  | UpdateInput Input
  | HandleNewExprButton NewBtn.Message

type State =
  { isActive :: Boolean 
  , scope :: Scope
  , mayIntroduceAdditionalFacts :: Boolean
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
    , scope: input.scope
    , mayIntroduceAdditionalFacts: input.mayIntroduceAdditionalFacts
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
              [ HH.h1 [ HP.class_ (ClassName "title") ] [ HH.text "use rule" ]
              ]
            , HH.div
              [ HP.class_ (ClassName "box") ] 
              [ HH.h1 [ HP.class_ (ClassName "subtitle") ] [ HH.text "facts in scope" ]
              , showFacts state.scope 
              ]
            , HH.div
              [ HP.class_ (ClassName "box") ] 
              [ HH.h1 [ HP.class_ (ClassName "subtitle") ] [ HH.text "you might add additional facts" ]
              , HH.slot _newExpr unit NewBtn.component unit (Just <<< HandleNewExprButton)
              ]
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
      H.modify_ (_ { scope = input.scope })
    HandleNewExprButton (NewBtn.NewExpr expr) ->
      H.modify_ (\st -> st { scope = Scope.include st.scope expr })

  showFacts scope = HH.div
    [ HP.class_ (ClassName "buttons is-marginless") ]
    (map showFact $ Scope.toArray scope)
  showFact expr = HH.button 
    [ HP.class_ (ClassName "button") ] 
    [ HH.text $ show expr ]