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

type DB = Ref (Array User)

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

userIndex :: DB -> Aff (Array User)
userIndex usersRef = liftEffect (Ref.read usersRef)

userShow :: DB -> String -> Aff (Maybe User)
userShow usersRef id = do
  users <- liftEffect (Ref.read usersRef)
  pure (Array.find ((eq id) <<< _.id) users)

userCreate :: DB -> User -> Aff (Maybe User)
userCreate usersRef user = do
  users <- liftEffect (Ref.read usersRef)
  case Array.find ((eq user.id) <<< _.id) users of
    Maybe.Just _ -> pure Maybe.Nothing
    Maybe.Nothing -> do
      _ <- liftEffect (Ref.write (Array.cons user users) usersRef)
      pure (Maybe.Just user)

userUpdate :: DB -> String -> User -> Aff (Maybe User)
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

userDestroy :: DB -> String -> Aff Boolean
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

userIndexAction :: DB -> HTTPure.ResponseM
userIndexAction usersRef = do
  users <- userIndex usersRef
  HTTPure.ok (SimpleJSON.writeJSON users)

userCreateAction :: DB -> String -> HTTPure.ResponseM
userCreateAction usersRef body = do
  case SimpleJSON.readJSON_ body :: _ User of
    Maybe.Nothing -> HTTPure.badRequest body
    Maybe.Just user -> do
      created <- userCreate usersRef user
      if Maybe.isJust created
        then HTTPure.ok (SimpleJSON.writeJSON user)
        else HTTPure.badRequest body

userShowAction :: DB -> String -> HTTPure.ResponseM
userShowAction usersRef id = do
  userMaybe <- userShow usersRef id
  case userMaybe of
    Maybe.Nothing -> HTTPure.notFound
    Maybe.Just user -> HTTPure.ok (SimpleJSON.writeJSON user)

userUpdateAction :: DB -> String -> String -> HTTPure.ResponseM
userUpdateAction usersRef id body = do
  case SimpleJSON.readJSON_ body :: _ User of
    Maybe.Nothing -> HTTPure.badRequest body
    Maybe.Just user -> do
      updated <- userUpdate usersRef id user
      if Maybe.isJust updated
        then HTTPure.ok (SimpleJSON.writeJSON user)
        else HTTPure.notFound

userDestroyAction :: DB -> String -> HTTPure.ResponseM
userDestroyAction usersRef id = do
  deleted <- userDestroy usersRef id
  if deleted
    then HTTPure.noContent
    else HTTPure.notFound

router :: DB -> HTTPure.Request -> HTTPure.ResponseM
router db = case _ of
  { method: HTTPure.Get, path: ["users"] } -> userIndexAction db
  { method: HTTPure.Post, path: ["users"], body } -> userCreateAction db body
  { method: HTTPure.Get, path: ["users", id] } -> userShowAction db id
  { method: HTTPure.Patch, path: ["users", id], body } ->
    userUpdateAction db id body
  { method: HTTPure.Delete, path: ["users", id] } -> userDestroyAction db id
  _ -> HTTPure.notFound

main :: HTTPure.ServerM
main = do
  db <- Ref.new initialUsers
  HTTPure.serve port (router db) booted
  where
    booted :: Effect Unit
    booted = Console.log "Server now up on port 8080"

    port :: Int
    port = 8080
