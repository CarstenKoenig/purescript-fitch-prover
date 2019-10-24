module Description 
  ( Description, Part
  , desc
  , text
  , expr
  , RenderConfig
  , renderDesc
  , stringConfig
  ) where

import Prelude

import Data.Foldable (foldMap, intercalate)
import Data.Newtype (class Newtype)
import Data.Expressions (Expr)

data Part
  = Text String
  | Expr Expr

derive instance eqPart :: Eq Part

newtype Description = Description (Array Part)

derive instance eqDescription :: Eq Description

derive instance newtypeDescription :: Newtype Description _
instance showDescription :: Show Description where
  show = renderDesc stringConfig

desc :: Array Part -> Description
desc = Description

text :: String -> Part
text = Text

expr :: Expr -> Part
expr = Expr

type RenderConfig a out =
  { text :: String -> a
  , expr :: Expr -> a
  , wrap :: Array a -> out
  }

renderDesc :: forall a out. RenderConfig a out -> Description -> out
renderDesc config (Description parts) =
  config.wrap $ foldMap render parts
  where
    render (Text s) = pure $ config.text s
    render (Expr e) = pure $ config.expr e


stringConfig :: RenderConfig String String
stringConfig =
  { text: identity
  , expr: show
  , wrap: intercalate " "
  }