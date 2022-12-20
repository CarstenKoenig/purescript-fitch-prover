module Main where

import Prelude

import Components.Router as R
import Data.Maybe (Maybe(..))
import Data.Route as Route
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex as RD
import Routing.Hash (matchesWith)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  halogenIO <- runUI R.component {} body

  void $ liftEffect $ matchesWith (RD.parse Route.codec) \old new -> do
    when (old /= Just new) do
      launchAff_ $ void $ halogenIO.query $ H.mkTell $ R.Navigate new