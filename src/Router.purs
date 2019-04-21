module Router
  ( router
  ) where

import Action.User as UserAction
import HTTPure as HTTPure
import Type (DB)

router :: DB -> HTTPure.Request -> HTTPure.ResponseM
router db = case _ of
  { method: HTTPure.Get, path: ["users"] } ->
    UserAction.index db
  { method: HTTPure.Post, path: ["users"], body } ->
    UserAction.create db body
  { method: HTTPure.Get, path: ["users", id] } ->
    UserAction.show db id
  { method: HTTPure.Patch, path: ["users", id], body } ->
    UserAction.update db id body
  { method: HTTPure.Delete, path: ["users", id] } ->
    UserAction.destroy db id
  _ -> HTTPure.notFound
