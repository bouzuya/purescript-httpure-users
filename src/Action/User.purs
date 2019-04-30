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
import View.User as UserView

index :: DB -> HTTPure.ResponseM
index db = do
  users <- UserModel.index db
  HTTPure.ok (UserView.index users)

create :: DB -> String -> HTTPure.ResponseM
create db body = do
  case SimpleJSON.readJSON_ body :: _ User of
    Maybe.Nothing -> HTTPure.badRequest body
    Maybe.Just user -> do
      created <- UserModel.create db user
      if Maybe.isJust created
        then HTTPure.ok (UserView.show user)
        else HTTPure.badRequest body

show :: DB -> String -> HTTPure.ResponseM
show db id = do
  userMaybe <- UserModel.show db id
  case userMaybe of
    Maybe.Nothing -> HTTPure.notFound
    Maybe.Just user -> HTTPure.ok (UserView.show user)

update :: DB -> String -> String -> HTTPure.ResponseM
update db id body = do
  case SimpleJSON.readJSON_ body :: _ User of
    Maybe.Nothing -> HTTPure.badRequest body
    Maybe.Just user -> do
      updated <- UserModel.update db id user
      if Maybe.isJust updated
        then HTTPure.ok (UserView.show user)
        else HTTPure.notFound

destroy :: DB -> String -> HTTPure.ResponseM
destroy db id = do
  deleted <- UserModel.destroy db id
  if deleted
    then HTTPure.noContent
    else HTTPure.notFound
