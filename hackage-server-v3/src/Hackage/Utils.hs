{-# LANGUAGE OverloadedStrings #-}

module Hackage.Utils
  ( module Hackage.Utils
  , ServerM
  , Connection
  , SessionError
  ) where

import Control.Exception (bracket)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Hackage.ServerM
import Hasql.Connection
import Hasql.Connection.Setting qualified as DB
import Hasql.Session (SessionError, statement, run)
import Rel8 hiding (null, run, Enum)
import Rel8 qualified as Rel8
import Servant.Server


withConn :: [DB.Setting] -> (Connection -> IO a) ->  IO a
withConn ss = bracket (acquire ss >>= either (error . show) pure) release


liftDB :: (Connection -> IO (Either SessionError a)) -> ServerM a
liftDB ma = do
  withConnection $ \conn ->
    liftIO (ma conn) >>= \case
      Left _err ->
        throwError $ err500 { errBody = BSL8.pack $ show _err }
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

