module Main
  ( main
  ) where

import Prelude

import Data.Foldable as Foldable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class as Class
import Effect.Console as Console
import HTTPure as HTTPure
import Model.DB as DB
import Router as Router
import Type (User, DB)

initialUsers :: Array User
initialUsers =
  [ { id: "1", name: "bouzuya" }
  , { id: "2", name: "user1" }
  , { id: "3", name: "user2" }
  ]

main :: Effect Unit
main = Aff.launchAff_ do
  db <- initialDB
  Class.liftEffect (HTTPure.serve port (Router.router db) booted)
  where
    initialDB :: Aff DB
    initialDB = do
      db <- DB.empty
      Foldable.for_ initialUsers \user -> do
        DB.create db user.id user
      pure db

    booted :: Effect Unit
    booted = Console.log "Server now up on port 8080"

    port :: Int
    port = 8080
