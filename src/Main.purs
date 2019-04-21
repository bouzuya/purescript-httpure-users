module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Effect.Ref as Ref
import HTTPure as HTTPure
import Router as Router
import Type (User)

initialUsers :: Array User
initialUsers =
  [ { id: "1", name: "bouzuya" }
  , { id: "2", name: "user1" }
  , { id: "3", name: "user2" }
  ]

main :: HTTPure.ServerM
main = do
  db <- Ref.new initialUsers
  HTTPure.serve port (Router.router db) booted
  where
    booted :: Effect Unit
    booted = Console.log "Server now up on port 8080"

    port :: Int
    port = 8080
