{-# LANGUAGE OverloadedStrings #-}

module Hackage.Utils
  ( module Hackage.Utils
  , Connection
  ) where

import Hasql.Connection (Connection)
import Hasql.Session (SessionError, statement, run)
import Servant.Server
import Control.Monad.IO.Class
import Control.Monad.Except
import Rel8 hiding (null, run, Enum)
import qualified Rel8 as Rel8


toE :: IO (Either SessionError a) -> Handler a
toE ma =
  liftIO ma >>= \case
    Left _err ->
      throwError $ err500 { errBody = "A database exception occurred!" }
    Right a -> pure a


doSelect
    :: Serializable exprs (FromExprs exprs)
    => Connection
    -> Query exprs
    -> IO (Either SessionError [FromExprs exprs])
doSelect conn = flip run conn . statement () . Rel8.run . select


doSelect1
    :: Serializable exprs (FromExprs exprs)
    => Connection
    -> Query exprs
    -> IO (Either SessionError (FromExprs exprs))
doSelect1 conn = flip run conn . statement () . Rel8.run1 . select


doSelectE
    :: Serializable exprs (FromExprs exprs)
    => Connection
    -> Query exprs
    -> Handler [FromExprs exprs]
doSelectE conn = toE . doSelect conn


doSelect1E
    :: Serializable exprs (FromExprs exprs)
    => Connection
    -> Query exprs
    -> Handler (FromExprs exprs)
doSelect1E conn = toE . doSelect1 conn


doUpdate
    :: Serializable exprs (FromExprs exprs)
    => Connection
    -> Update (Query exprs)
    -> IO (Either SessionError [FromExprs exprs])
doUpdate conn = flip run conn . statement () . Rel8.run . update


doUpdate_
    :: Connection
    -> Update a
    -> IO (Either SessionError ())
doUpdate_ conn = flip run conn . statement () . Rel8.run_ . update


doUpdate1
    :: Serializable exprs (FromExprs exprs)
    => Connection
    -> Update (Query exprs)
    -> IO (Either SessionError (FromExprs exprs))
doUpdate1 conn = flip run conn . statement () . Rel8.run1 . update


doUpdateE
    :: Serializable exprs (FromExprs exprs)
    => Connection
    -> Update (Query exprs)
    -> Handler [FromExprs exprs]
doUpdateE conn = toE . doUpdate conn


doInsert
    :: Serializable exprs (FromExprs exprs)
    => Connection
    -> Insert (Query exprs)
    -> IO (Either SessionError [FromExprs exprs])
doInsert conn = flip run conn . statement () . Rel8.run . insert


doInsert1
    :: Serializable exprs (FromExprs exprs)
    => Connection
    -> Insert (Query exprs)
    -> IO (Either SessionError (FromExprs exprs))
doInsert1 conn = flip run conn . statement () . Rel8.run1 . insert


doInsertE
    :: Serializable exprs (FromExprs exprs)
    => Connection
    -> Insert (Query exprs)
    -> Handler [FromExprs exprs]
doInsertE conn = toE . doInsert conn


doInsert_
    :: Connection
    -> Insert a
    -> IO (Either SessionError ())
doInsert_ conn = flip run conn . statement () . Rel8.run_ . insert


doInsertE_
    :: Connection
    -> Insert a
    -> Handler ()
doInsertE_ conn = toE . doInsert_ conn


doDelete
    :: Serializable exprs (FromExprs exprs)
    => Connection
    -> Delete (Query exprs)
    -> IO (Either SessionError [FromExprs exprs])
doDelete conn = flip run conn . statement () . Rel8.run . delete


doDeleteE
    :: Serializable exprs (FromExprs exprs)
    => Connection
    -> Delete (Query exprs)
    -> Handler [FromExprs exprs]
doDeleteE conn = toE . doDelete conn


doDelete_ :: Connection -> Delete a -> IO (Either SessionError ())
doDelete_ conn = flip run conn . statement () . Rel8.run_ . delete


doDeleteE_ :: Connection -> Delete a -> Handler ()
doDeleteE_ conn = toE . doDelete_ conn

