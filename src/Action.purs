module Action
  ( Action(..)
  , UserId
  , execute
  ) where

import Action.User as UserAction
import HTTPure (ResponseM)
import Type (User, DB)

type UserId = String

data Action
  = UserIndex
  | UserCreate User
  | UserShow UserId
  | UserUpdate UserId User
  | UserDestroy UserId

execute :: DB -> Action -> ResponseM
execute db =
  case _ of
    UserIndex -> UserAction.index db
    UserCreate user -> UserAction.create db user
    UserShow id -> UserAction.show db id
    UserUpdate id user -> UserAction.update db id user
    UserDestroy id -> UserAction.destroy db id
