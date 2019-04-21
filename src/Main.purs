module Main
  ( main
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Aff (Aff)
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

userIndex :: Ref (Array User) -> Aff (Array User)
userIndex usersRef = liftEffect (Ref.read usersRef)

userShow :: Ref (Array User) -> String -> Aff (Maybe User)
userShow usersRef id = do
  users <- liftEffect (Ref.read usersRef)
  pure (Array.find ((eq id) <<< _.id) users)

userUpdate :: Ref (Array User) -> String -> User -> Aff (Maybe User)
userUpdate usersRef id user = do
  users <- liftEffect (Ref.read usersRef)
  case Array.findIndex ((eq id) <<< _.id) users of
    Maybe.Nothing -> pure Maybe.Nothing
    Maybe.Just index ->
      case Array.updateAt index user users of
        Maybe.Nothing -> pure Maybe.Nothing
        Maybe.Just users' -> do
          _ <- liftEffect (Ref.write users' usersRef)
          pure (Maybe.Just user)

userDestroy :: Ref (Array User) -> String -> Aff Boolean
userDestroy usersRef id = do
  users <- liftEffect (Ref.read usersRef)
  case Array.findIndex ((eq id) <<< _.id) users of
    Maybe.Nothing -> pure false
    Maybe.Just index ->
      case Array.deleteAt index users of
        Maybe.Nothing -> pure false
        Maybe.Just users' -> do
          _ <- liftEffect (Ref.write users' usersRef)
          pure true

router :: Ref (Array User) -> HTTPure.Request -> HTTPure.ResponseM
router usersRef { method: HTTPure.Get, path: ["users"] } = do
  users <- userIndex usersRef
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
  userMaybe <- userShow usersRef id
  case userMaybe of
    Maybe.Nothing -> HTTPure.notFound
    Maybe.Just user -> HTTPure.ok (SimpleJSON.writeJSON user)
router usersRef { method: HTTPure.Patch, path: ["users", id], body } = do
  case SimpleJSON.readJSON_ body :: _ User of
    Maybe.Nothing -> HTTPure.badRequest body
    Maybe.Just user -> do
      updated <- userUpdate usersRef id user
      if Maybe.isJust updated
        then HTTPure.ok (SimpleJSON.writeJSON user)
        else HTTPure.notFound
router usersRef { method: HTTPure.Delete, path: ["users", id] } = do
  deleted <- userDestroy usersRef id
  if deleted
    then HTTPure.noContent
    else HTTPure.notFound
router _ _ = HTTPure.notFound

main :: HTTPure.ServerM
main = do
  usersRef <- Ref.new initialUsers
  HTTPure.serve port (router usersRef) booted
  where
    booted :: Effect Unit
    booted = Console.log "Server now up on port 8080"

    port :: Int
    port = 8080
