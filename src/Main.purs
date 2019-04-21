module Main
  ( main
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import HTTPure as HTTPure
import Simple.JSON as SimpleJSON

type User =
  { id :: String
  , name :: String
  }

initialUsers :: Array User
initialUsers =
  [ { id: "1", name: "bouzuya" }
  , { id: "2", name: "user1" }
  , { id: "3", name: "user2" }
  ]

main :: HTTPure.ServerM
main = do
  usersRef <- Ref.new initialUsers
  HTTPure.serve port (router usersRef) booted
  where
    booted :: Effect Unit
    booted = Console.log "Server now up on port 8080"

    port :: Int
    port = 8080

    router :: Ref (Array User) -> HTTPure.Request -> HTTPure.ResponseM
    router usersRef { method: HTTPure.Get, path: ["users"] } = do
        users <- liftEffect (Ref.read usersRef)
        HTTPure.ok (SimpleJSON.writeJSON users)
    router usersRef { method: HTTPure.Post, path: ["users"], body } = do
      users <- liftEffect (Ref.read usersRef)
      case SimpleJSON.readJSON_ body :: _ User of
        Maybe.Nothing -> HTTPure.badRequest body
        Maybe.Just user ->
          case Array.find ((eq user.id) <<< _.id) users of
            Maybe.Just _ -> HTTPure.badRequest body
            Maybe.Nothing -> do
              _ <- liftEffect (Ref.write (Array.cons user users) usersRef)
              HTTPure.ok (SimpleJSON.writeJSON user)
    router usersRef { method: HTTPure.Get, path: ["users", id] } = do
      users <- liftEffect (Ref.read usersRef)
      case Array.find ((eq id) <<< _.id) users of
        Maybe.Nothing -> HTTPure.notFound
        Maybe.Just user -> HTTPure.ok (SimpleJSON.writeJSON user)
    router _ _ = HTTPure.ok "Hello, world!"
