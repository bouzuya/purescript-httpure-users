module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import HTTPure as HTTPure

type User =
  { name :: String
  }

users :: Array User
users =
  [ { name: "bouzuya" }
  , { name: "user1" }
  , { name: "user2" }
  ]

main :: HTTPure.ServerM
main = HTTPure.serve port router booted
  where
    booted :: Effect Unit
    booted = Console.log "Server now up on port 8080"

    port :: Int
    port = 8080

    router :: HTTPure.Request -> HTTPure.ResponseM
    router _ = HTTPure.ok "Hello, world!"
