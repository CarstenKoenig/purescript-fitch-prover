{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "generics-rep"
    , "halogen"
    , "parsing"
    , "psci-support"
    , "routing"
    , "routing-duplex"
    , "transformers"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
