module Model.DB
  ( DB
  , Id
  , empty
  , index
  , create
  , show
  , update
  , destroy
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Effect.Aff (Aff)
import Effect.Class as Class
import Effect.Ref (Ref)
import Effect.Ref as Ref

type DB a = Ref (Array (Tuple Id a))
type Id = String

empty :: forall a. Aff (DB a)
empty = Class.liftEffect (Ref.new [])

index :: forall a. DB a -> Aff (Array a)
index db = Class.liftEffect (map (map Tuple.snd) (Ref.read db))

show :: forall a. DB a -> Id -> Aff (Maybe a)
show db id = do
  items <- Class.liftEffect (Ref.read db)
  pure (map Tuple.snd (Array.find ((eq id) <<< Tuple.fst) items))

create :: forall a. DB a -> Id -> a -> Aff (Maybe a)
create db id item = do
  items <- Class.liftEffect (Ref.read db)
  case Array.find ((eq id) <<< Tuple.fst) items of
    Maybe.Just _ -> pure Maybe.Nothing
    Maybe.Nothing -> do
      _ <-
        Class.liftEffect (Ref.write (Array.cons (Tuple.Tuple id item) items) db)
      pure (Maybe.Just item)

update :: forall a. DB a -> Id -> a -> Aff (Maybe a)
update db id item = do
  items <- Class.liftEffect (Ref.read db)
  case Array.findIndex ((eq id) <<< Tuple.fst) items of
    Maybe.Nothing -> pure Maybe.Nothing
    Maybe.Just index' ->
      case Array.updateAt index' (Tuple.Tuple id item) items of
        Maybe.Nothing -> pure Maybe.Nothing
        Maybe.Just items' -> do
          _ <- Class.liftEffect (Ref.write items' db)
          pure (Maybe.Just item)

destroy :: forall a. DB a -> Id -> Aff Boolean
destroy db id = do
  items <- Class.liftEffect (Ref.read db)
  case Array.findIndex ((eq id) <<< Tuple.fst) items of
    Maybe.Nothing -> pure false
    Maybe.Just index' ->
      case Array.deleteAt index' items of
        Maybe.Nothing -> pure false
        Maybe.Just items' -> do
          _ <- Class.liftEffect (Ref.write items' db)
          pure true
