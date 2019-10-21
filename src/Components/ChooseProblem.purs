module Components.ChooseProblem (component) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Problem (Problem)
import Data.Problem as P
import Data.Route (navigate)
import Data.Route as R
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { problems :: Array Problem }

data Action
  = GotoProblem Problem

type ChildSlots = ()

component :: forall q o m. MonadEffect m => H.Component HH.HTML q {} o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where

  initialState :: State
  initialState = 
    { problems:  P.problems }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = HH.div_
    [ showProblemButtons state.problems
    ]

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    GotoProblem p ->
      navigate (R.Problem p.number)

showProblemButtons :: forall w. Array Problem -> HTML w Action
showProblemButtons problems | Array.null problems = HH.text ""
showProblemButtons problems =
  HH.div
    [ HP.class_ (ClassName "box") ] 
    [ HH.h1 [ HP.class_ (ClassName "subtitle") ] [ HH.text "Problems" ]
    , HH.div
      [ HP.class_ (ClassName "buttons is-marginless") ]
      (map showProblemButton problems)
    ]
  where
  showProblemButton problem = HH.button 
    [ HP.class_ (ClassName "button") 
    , HE.onClick (\_ -> Just (GotoProblem problem))
    ] 
    [ HH.text problem.name ]