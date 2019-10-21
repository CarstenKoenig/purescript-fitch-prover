module Data.Route
  ( Route(..)
  , codec
 ) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', int, root, segment)
import Routing.Duplex.Generic (noArgs, sum)

data Route
  = Home
  | Problem Int

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

codec :: RouteDuplex' Route
codec = root $ sum
  { "Home": noArgs
  , "Problem": int segment
  }