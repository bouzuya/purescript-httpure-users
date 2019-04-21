module Action
  ( userIndexAction
  , userCreateAction
  , userShowAction
  , userUpdateAction
  , userDestroyAction
  ) where

import Prelude

import Data.Maybe as Maybe
import HTTPure as HTTPure
import Model as Model
import Simple.JSON as SimpleJSON
import Type (DB, User)

userIndexAction :: DB -> HTTPure.ResponseM
userIndexAction usersRef = do
  users <- Model.userIndex usersRef
  HTTPure.ok (SimpleJSON.writeJSON users)

userCreateAction :: DB -> String -> HTTPure.ResponseM
userCreateAction usersRef body = do
  case SimpleJSON.readJSON_ body :: _ User of
    Maybe.Nothing -> HTTPure.badRequest body
    Maybe.Just user -> do
      created <- Model.userCreate usersRef user
      if Maybe.isJust created
        then HTTPure.ok (SimpleJSON.writeJSON user)
        else HTTPure.badRequest body

userShowAction :: DB -> String -> HTTPure.ResponseM
userShowAction usersRef id = do
  userMaybe <- Model.userShow usersRef id
  case userMaybe of
    Maybe.Nothing -> HTTPure.notFound
    Maybe.Just user -> HTTPure.ok (SimpleJSON.writeJSON user)

userUpdateAction :: DB -> String -> String -> HTTPure.ResponseM
userUpdateAction usersRef id body = do
  case SimpleJSON.readJSON_ body :: _ User of
    Maybe.Nothing -> HTTPure.badRequest body
    Maybe.Just user -> do
      updated <- Model.userUpdate usersRef id user
      if Maybe.isJust updated
        then HTTPure.ok (SimpleJSON.writeJSON user)
        else HTTPure.notFound

userDestroyAction :: DB -> String -> HTTPure.ResponseM
userDestroyAction usersRef id = do
  deleted <- Model.userDestroy usersRef id
  if deleted
    then HTTPure.noContent
    else HTTPure.notFound
