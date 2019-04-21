module Model
  ( userIndex
  , userCreate
  , userShow
  , userUpdate
  , userDestroy
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

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
