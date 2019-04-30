module Type
  ( DB
  , User
  ) where

import Model.DB as Model

type DB = Model.DB User

type User =
  { id :: String
  , name :: String
  }
