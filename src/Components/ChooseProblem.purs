module Components.ChooseProblem (component) where

import Prelude

import Data.Foldable (null)
import Data.Problem (Problem, ProblemNumber, Problems)
import Data.Problem as P
import Data.Route (navigate)
import Data.Route as R
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { problems :: Problems }

data Action = GotoProblem ProblemNumber

type ChildSlots :: forall k. Row k
type ChildSlots = ()

component :: forall q o m. MonadEffect m => H.Component q {} o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where

  initialState :: State
  initialState =
    { problems: P.problems }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = HH.div_
    [ showProblemButtons $ P.toUnfoldable state.problems ]

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    GotoProblem nr ->
      navigate (R.Problem nr)

showProblemButtons :: forall w. Array (Tuple ProblemNumber Problem) -> HTML w Action
showProblemButtons problems | null problems = HH.text ""
showProblemButtons problems =
  HH.div
    [ HP.class_ (ClassName "box") ]
    [ HH.h1 [ HP.class_ (ClassName "subtitle") ] [ HH.text "Problems" ]
    , HH.div
        [ HP.class_ (ClassName "buttons is-marginless") ]
        (map showProblemButton problems)
    ]
  where
  showProblemButton (Tuple nr problem) = HH.button
    [ HP.class_ (ClassName "button")
    , HE.onClick (\_ -> GotoProblem nr)
    ]
    [ HH.text problem.name ]