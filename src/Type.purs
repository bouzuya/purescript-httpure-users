module Type
  ( DB
  , User
  ) where

import Effect.Ref (Ref)

type DB = Ref (Array User)

type User =
  { id :: String
  , name :: String
  }
