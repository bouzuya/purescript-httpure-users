module Router
  ( RouteError(..)
  , router
  ) where

import Prelude

import Action (Action(..))
import Data.Either (Either)
import Data.Either as Either
import HTTPure as HTTPure
import Simple.JSON (class ReadForeign)
import Simple.JSON as SimpleJSON

data RouteError
  = ClientError String
  | NotFound

fromJSON :: forall a. ReadForeign a => String -> Either RouteError a
fromJSON s =
  Either.either
    (Either.Left <<< ClientError <<< show)
    Either.Right
    (SimpleJSON.readJSON s)

router :: HTTPure.Request -> Either RouteError Action
router = case _ of
  { method: HTTPure.Get, path: ["users"] } -> pure UserIndex
  { method: HTTPure.Post, path: ["users"], body } -> do
    user <- fromJSON body
    pure (UserCreate user)
  { method: HTTPure.Get, path: ["users", id] } -> pure (UserShow id)
  { method: HTTPure.Patch, path: ["users", id], body } -> do
    user <- fromJSON body
    pure (UserUpdate id user)
  { method: HTTPure.Delete, path: ["users", id] } -> pure (UserDestroy id)
  _ -> Either.Left NotFound
