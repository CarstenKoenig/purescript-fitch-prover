module MathJaxRenderer where

import Prelude

import Data.Expressions (RenderConfig, Expr, render)
import Description (Description)
import Description as Desc
import Effect (Effect)
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

showMathJax :: forall w i. Expr -> HTML w i
showMathJax = render mathJaxConfig

showMathJaxDesc :: forall w i. Description -> HTML w i
showMathJaxDesc = Desc.renderDesc mathJaxDescConfig

mathJaxConfig :: forall w i. RenderConfig String (HTML w i)
mathJaxConfig =
  { wrap: \formula -> HH.span [ HP.class_ (ClassName "math") ] [ HH.text $ "\\( " <> formula <> " \\)" ]
  , and: \a b -> a <> " \\land " <> b
  , or: \a b -> a <> " \\lor " <> b
  , impl: \a b -> a <> " \\implies " <> b
  , not: \a -> "\\neg " <> a
  , inBraces: \a -> " \\left( " <> a <> " \\right) "
  , renderString: identity
  }

foreign import typeSetPage :: Effect Unit

mathJaxDescConfig :: forall w i. Desc.RenderConfig (HTML w i) (HTML w i)
mathJaxDescConfig =
  { text: HH.text
  , expr: showMathJax
  , wrap: HH.span [ HP.class_ (ClassName "description") ]
  }