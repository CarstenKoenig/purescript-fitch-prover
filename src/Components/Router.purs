module Components.Router where

import Prelude hiding ((/))

import Components.ChooseProblem as CP
import Components.SolveProblem as SP
import Data.Const (Const)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Problem as P
import Data.Route (Route, navigate)
import Data.Route as Route
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Routing.Duplex as RD
import Routing.Hash (getHash)

type State =
  { route :: Maybe Route
  }

data Query a
  = Navigate Route a

data Action 
  = Initialize 
  
type OpaqueSlot = H.Slot (Const Void) Void

type ChildSlots = 
  ( home :: OpaqueSlot Unit
  , problem :: OpaqueSlot Unit
  )

component :: forall m . MonadAff m => H.Component HH.HTML Query {} Void m
component = H.mkComponent
  { initialState: \_ -> { route: Nothing } 
  , render
  , eval: H.mkEval $ H.defaultEval 
      { handleQuery = handleQuery 
      , handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where 
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      -- first we'll get the route the user landed on
      initialRoute <- hush <<< (RD.parse Route.codec) <$> liftEffect getHash
      -- then we'll navigate to the new route (also setting the hash)
      navigate $ fromMaybe Route.Home initialRoute

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- H.get 
      -- don't re-render unnecessarily if the route is unchanged
      when (route /= Just dest) do
          H.modify_ _ { route = Just dest }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = case route of
    Just r -> case r of
      Route.Home -> 
        HH.slot (SProxy :: _ "home") unit CP.component {} absurd
      Route.Problem p -> 
        case P.getProblem p of
          Nothing ->
            HH.div_ [ HH.text "Oh no! I don't know this problem." ]
          Just problem ->
            HH.slot (SProxy :: _ "problem") unit SP.component problem absurd
    Nothing ->
      HH.div_ [ HH.text "Oh no! That page wasn't found." ]