module Components.NewExprButton where

import Prelude

import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Expressions (Expr, tryParse)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Text.Parsing.Parser (ParseError(..), parseErrorMessage)
import Text.Parsing.Parser.Pos (initialPos)
import Web.Event.Event (Event, preventDefault)
import Web.HTML.HTMLElement (focus)

type Slot = H.Slot (Const Unit) Message

data Message = NewExpr Expr

data Action 
  = Toggle
  | ChangeInput String
  | Cancel
  | Submit Event

data State 
  = Inactive
  | Inputting { input :: String, result :: Either ParseError Expr }


component :: forall q i m. MonadEffect m => H.Component HH.HTML q i Message m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      }
    }
  where

  initialState :: State
  initialState = Inactive

  render :: State -> H.ComponentHTML Action () m
  render Inactive =
      HH.div
        [ HP.class_ $ ClassName "field" ] 
        [ HH.div
          [ HP.class_ $ ClassName "control" ] 
          [ HH.button
            [ HP.title "new Expr"
            , HP.class_ (ClassName "button is-primary")
            , HE.onClick (\_ -> Just Toggle)
            ]
            [ HH.text "new Expr" ]
          ]
        ]
  render (Inputting inputState) =
    HH.form
      [ HE.onSubmit (\ev -> Just $ Submit ev)]
      [ HH.div
        [ HP.class_ $ ClassName "field" ] 
        [ HH.div
          [ HP.class_ $ ClassName "control" ] 
          [ HH.input
            [ HP.title "enter an expression pls"
            , HE.onValueInput (\newInput -> Just (ChangeInput newInput))
            , HE.onFocusOut (\_ -> Just Cancel)
            , HP.value inputState.input
            , HP.autofocus true
            , HP.ref (H.RefLabel "input")
            , HP.classes $ map ClassName [ "input", either (const "is-danger") (const "is-success") inputState.result ]
            ]
          ]
        , either 
            (\err -> HH.p [ HP.class_ $ ClassName "help is-danger"] [ HH.text $ parseErrorMessage err ]) 
            (const $ HH.text "") 
            inputState.result
        ]
      ]

  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    Toggle -> do
      H.modify_ (case _ of
        Inactive -> Inputting { input: "", result: Left (ParseError "no input" initialPos)}
        Inputting _ -> Inactive
      )
      -- focus on input
      H.getHTMLElementRef (H.RefLabel "input") >>= traverse_ (liftEffect <<< focus)
    Cancel -> 
      H.modify_ (const Inactive)
    ChangeInput newInput -> H.modify_ (case _ of
        Inactive -> Inactive
        Inputting state -> Inputting $ state { input = newInput, result = tryParse newInput }
      )
    Submit ev -> do
      liftEffect (preventDefault ev)
      res <- H.gets (case _ of
        Inactive -> Nothing
        Inputting state -> either (const Nothing) Just state.result
      )
      case res of
        Nothing -> 
          pure unit
        Just expr -> do
          H.raise (NewExpr expr)
          H.put Inactive