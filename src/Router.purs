module Router
  ( router
  ) where

import Action as Action
import HTTPure as HTTPure
import Type (DB)

router :: DB -> HTTPure.Request -> HTTPure.ResponseM
router db = case _ of
  { method: HTTPure.Get, path: ["users"] } ->
    Action.userIndexAction db
  { method: HTTPure.Post, path: ["users"], body } ->
    Action.userCreateAction db body
  { method: HTTPure.Get, path: ["users", id] } ->
    Action.userShowAction db id
  { method: HTTPure.Patch, path: ["users", id], body } ->
    Action.userUpdateAction db id body
  { method: HTTPure.Delete, path: ["users", id] } ->
    Action.userDestroyAction db id
  _ -> HTTPure.notFound
