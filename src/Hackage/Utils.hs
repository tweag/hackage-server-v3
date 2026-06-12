{-# LANGUAGE OverloadedStrings #-}

module Hackage.Utils
  ( module Hackage.Utils
  , ServerM
  , Connection
  ) where

import Hasql.Connection (Connection)
import Hasql.Session (SessionError, statement, run)
import Servant.Server
import Control.Monad.IO.Class
import Control.Monad.Except
import Rel8 hiding (null, run, Enum)
import qualified Rel8 as Rel8
import Hackage.ServerM


liftDB :: (Connection -> IO (Either SessionError a)) -> ServerM a
liftDB ma = do
  withConnection $ \conn ->
    liftIO (ma conn) >>= \case
      Left _err ->
        throwError $ err500 { errBody = "A database exception occurred!" }
      Right a -> pure a


doSelect
    :: Serializable exprs (FromExprs exprs)
    => Query exprs
    -> Connection
    -> IO (Either SessionError [FromExprs exprs])
doSelect = run . statement () . Rel8.run . select


doSelect1
    :: Serializable exprs (FromExprs exprs)
    => Query exprs
    -> Connection
    -> IO (Either SessionError (FromExprs exprs))
doSelect1 = run . statement () . Rel8.run1 . select



doUpdate
    :: Serializable exprs (FromExprs exprs)
    => Update (Query exprs)
    -> Connection
    -> IO (Either SessionError [FromExprs exprs])
doUpdate = run . statement () . Rel8.run . update


doUpdate_
    :: Update a
    -> Connection
    -> IO (Either SessionError ())
doUpdate_ = run . statement () . Rel8.run_ . update


doInsert
    :: Serializable exprs (FromExprs exprs)
    => Insert (Query exprs)
    -> Connection
    -> IO (Either SessionError [FromExprs exprs])
doInsert = run . statement () . Rel8.run . insert


doInsert1
    :: Serializable exprs (FromExprs exprs)
    => Insert (Query exprs)
    -> Connection
    -> IO (Either SessionError (FromExprs exprs))
doInsert1 = run . statement () . Rel8.run1 . insert


doInsert_
    :: Insert a
    -> Connection
    -> IO (Either SessionError ())
doInsert_ = run . statement () . Rel8.run_ . insert


doDelete
    :: Serializable exprs (FromExprs exprs)
    => Delete (Query exprs)
    -> Connection
    -> IO (Either SessionError [FromExprs exprs])
doDelete = run . statement () . Rel8.run . delete


doDelete_ :: Delete a -> Connection -> IO (Either SessionError ())
doDelete_ = run . statement () . Rel8.run_ . delete

