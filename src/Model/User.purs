module Model.User
  ( index
  , create
  , show
  , update
  , destroy
  ) where

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Model.DB as DB
import Type (DB, User)

index :: DB -> Aff (Array User)
index = DB.index

show :: DB -> String -> Aff (Maybe User)
show = DB.show

create :: DB -> User -> Aff (Maybe User)
create db user = DB.create db user.id user

update :: DB -> String -> User -> Aff (Maybe User)
update = DB.update

destroy :: DB -> String -> Aff Boolean
destroy = DB.destroy
