module Main
  ( main
  ) where

import Prelude

import Action as Action
import Data.Either as Either
import Data.Foldable as Foldable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class as Class
import Effect.Console as Console
import HTTPure (Request, ResponseM)
import HTTPure as HTTPure
import Model.DB as DB
import Router as Router
import Type (User, DB)

initialUsers :: Array User
initialUsers =
  [ { id: "1", name: "bouzuya" }
  , { id: "2", name: "user1" }
  , { id: "3", name: "user2" }
  ]

main :: Effect Unit
main = Aff.launchAff_ do
  db <- initialDB
  Class.liftEffect (HTTPure.serve port (app db) booted)
  where
    initialDB :: Aff DB
    initialDB = do
      db <- DB.empty
      Foldable.for_ initialUsers \user -> do
        DB.create db user.id user
      pure db

    app :: DB -> Request -> ResponseM
    app db request =
      case Router.router request of
        Either.Right action -> Action.execute db action
        Either.Left (Router.ClientError _) ->
          HTTPure.badRequest "invalid params"
        Either.Left Router.NotFound ->
          HTTPure.notFound

    booted :: Effect Unit
    booted = Console.log "Server now up on port 8080"

    port :: Int
    port = 8080
