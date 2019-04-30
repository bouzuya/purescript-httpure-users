module Action.User
  ( index
  , create
  , show
  , update
  , destroy
  ) where

import Prelude

import Data.Maybe as Maybe
import HTTPure as HTTPure
import Model.User as UserModel
import Simple.JSON as SimpleJSON
import Type (DB, User)

index :: DB -> HTTPure.ResponseM
index usersRef = do
  users <- UserModel.index usersRef
  HTTPure.ok (SimpleJSON.writeJSON users)

create :: DB -> String -> HTTPure.ResponseM
create usersRef body = do
  case SimpleJSON.readJSON_ body :: _ User of
    Maybe.Nothing -> HTTPure.badRequest body
    Maybe.Just user -> do
      created <- UserModel.create usersRef user
      if Maybe.isJust created
        then HTTPure.ok (SimpleJSON.writeJSON user)
        else HTTPure.badRequest body

show :: DB -> String -> HTTPure.ResponseM
show usersRef id = do
  userMaybe <- UserModel.show usersRef id
  case userMaybe of
    Maybe.Nothing -> HTTPure.notFound
    Maybe.Just user -> HTTPure.ok (SimpleJSON.writeJSON user)

update :: DB -> String -> String -> HTTPure.ResponseM
update usersRef id body = do
  case SimpleJSON.readJSON_ body :: _ User of
    Maybe.Nothing -> HTTPure.badRequest body
    Maybe.Just user -> do
      updated <- UserModel.update usersRef id user
      if Maybe.isJust updated
        then HTTPure.ok (SimpleJSON.writeJSON user)
        else HTTPure.notFound

destroy :: DB -> String -> HTTPure.ResponseM
destroy usersRef id = do
  deleted <- UserModel.destroy usersRef id
  if deleted
    then HTTPure.noContent
    else HTTPure.notFound
