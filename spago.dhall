{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "const"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "strings"
  , "svg-parser-halogen"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unfoldable"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
