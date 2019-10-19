module Components.Workspace 
  ( Query (..)
  , Message (..)
  , Slot
  , component
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Expressions (Expr(..))
import FitchRules (RuleInstance, orIntroduction)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Slot = H.Slot Query Message

data Query a
  = IsOn (Boolean -> a)
  | SetTo Boolean a

data Message = Toggled Boolean

data Action = Toggle

type State = Boolean

----------------------------------------------------------------------
-- Display - Items

data DisplayItem
  = DisplayFact Expr
  | DisplayRule RuleInstance

renderItems :: forall w i. Array DisplayItem -> HTML w i
renderItems items = HH.ol_ $ map renderDisplayItem items 

renderDisplayItem :: forall w i. DisplayItem -> HTML w i
renderDisplayItem (DisplayFact expr) =
  HH.li_ [ HH.text (show expr) ]
renderDisplayItem (DisplayRule rule) =
  HH.li_ [ HH.text rule.description ]


----------------------------------------------------------------------
-- Definition of Halogen-component

component :: forall i m. H.Component HH.HTML Query i Message m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      , handleQuery = handleQuery
      }
    }
  where

  initialState :: State
  initialState = false

  render :: State -> H.ComponentHTML Action () m
  render state =
    let
      label = if state then "On" else "Off"
    in
      renderItems [ DisplayFact (SymbolExpr "a")
                  , DisplayRule (orIntroduction (SymbolExpr "a") (SymbolExpr "b"))
                  ]

  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    Toggle -> do
      newState <- H.modify not
      H.raise (Toggled newState)

  handleQuery :: forall a. Query a -> H.HalogenM State Action () Message m (Maybe a)
  handleQuery = case _ of
    SetTo state k -> do
      H.put state
      pure (Just k)
    IsOn k -> do
      enabled <- H.get
      pure (Just (k enabled))