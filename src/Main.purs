module Main
  ( main
  ) where

import Prelude

import Data.Foldable as Array
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Console as Console
import HTTPure as HTTPure
import Simple.JSON as SimpleJSON

type User =
  { id :: String
  , name :: String
  }

users :: Array User
users =
  [ { id: "1", name: "bouzuya" }
  , { id: "2", name: "user1" }
  , { id: "3", name: "user2" }
  ]

main :: HTTPure.ServerM
main = HTTPure.serve port router booted
  where
    booted :: Effect Unit
    booted = Console.log "Server now up on port 8080"

    port :: Int
    port = 8080

    router :: HTTPure.Request -> HTTPure.ResponseM
    router { path: ["users"] } = HTTPure.ok (SimpleJSON.writeJSON users)
    router _ = HTTPure.ok "Hello, world!"
