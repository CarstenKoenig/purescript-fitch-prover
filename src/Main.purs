module Main where

import Prelude

import Components.App as App
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Problem as P

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI App.component P.problem2 body