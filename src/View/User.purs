module View.User
  ( index
  , show
  ) where

import Simple.JSON as SimpleJSON
import Type (User)

index :: Array User -> String
index = SimpleJSON.writeJSON

show :: User -> String
show = SimpleJSON.writeJSON
