module Model.User
  ( index
  , create
  , show
  , update
  , destroy
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

index :: DB -> Aff (Array User)
index usersRef = liftEffect (Ref.read usersRef)

show :: DB -> String -> Aff (Maybe User)
show usersRef id = do
  users <- liftEffect (Ref.read usersRef)
  pure (Array.find ((eq id) <<< _.id) users)

create :: DB -> User -> Aff (Maybe User)
create usersRef user = do
  users <- liftEffect (Ref.read usersRef)
  case Array.find ((eq user.id) <<< _.id) users of
    Maybe.Just _ -> pure Maybe.Nothing
    Maybe.Nothing -> do
      _ <- liftEffect (Ref.write (Array.cons user users) usersRef)
      pure (Maybe.Just user)

update :: DB -> String -> User -> Aff (Maybe User)
update usersRef id user = do
  users <- liftEffect (Ref.read usersRef)
  case Array.findIndex ((eq id) <<< _.id) users of
    Maybe.Nothing -> pure Maybe.Nothing
    Maybe.Just index' ->
      case Array.updateAt index' user users of
        Maybe.Nothing -> pure Maybe.Nothing
        Maybe.Just users' -> do
          _ <- liftEffect (Ref.write users' usersRef)
          pure (Maybe.Just user)

destroy :: DB -> String -> Aff Boolean
destroy usersRef id = do
  users <- liftEffect (Ref.read usersRef)
  case Array.findIndex ((eq id) <<< _.id) users of
    Maybe.Nothing -> pure false
    Maybe.Just index' ->
      case Array.deleteAt index' users of
        Maybe.Nothing -> pure false
        Maybe.Just users' -> do
          _ <- liftEffect (Ref.write users' usersRef)
          pure true
