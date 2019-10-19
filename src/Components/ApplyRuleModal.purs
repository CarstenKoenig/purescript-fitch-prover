module Components.ApplyRuleModal
  ( Message (..)
  , Slot
  , Input
  , component
  ) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import FitchRules (RuleInstance)
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Scope (Scope)

type Slot = H.Slot (Const Unit) Message

data Message 
  = NewRule RuleInstance
  | Canceled

type Input =
  { scope :: Scope 
  }

data Action 
  = Close
  | UpdateInput Input

type State =
  { isActive :: Boolean 
  , scope :: Scope
  }


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
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
      HH.div
        [ HP.class_ $ ClassName $ if state.isActive then "modal is-active" else "modal" ] 
        [ HH.div [ HP.class_ $ ClassName "modal-background" ] []
        , HH.div [ HP.class_ $ ClassName "modal-content" ]
          []
        , HH.button
          [ HP.class_ (ClassName "modal-close is-large")
          , HP.attr (AttrName "aria-label") "close"
          , HE.onClick (\_ -> Just Close)
          ]
          []
        ]

  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    Close -> do
      H.modify_ (_ { isActive = false })
      H.raise Canceled
    UpdateInput input -> do
      H.modify_ (_ { scope = input.scope })