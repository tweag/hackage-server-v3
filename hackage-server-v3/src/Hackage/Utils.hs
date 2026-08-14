{-# LANGUAGE OverloadedStrings #-}

module Hackage.Utils
  ( module Hackage.Utils
  , ServerM
  , Connection
  , SessionError
  ) where

import Control.Exception (bracket)
import Control.Monad.Except
import Control.Monad.Reader
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


-- | A monad for performing database operations.
newtype DatabaseM a = DatabaseM
  { unDatabaseM :: ReaderT Connection (ExceptT SessionError IO) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO)


runDB :: DatabaseM a -> ServerM a
runDB ma = do
  withConnection $ \conn ->
    liftIO (runExceptT $ runReaderT (unDatabaseM ma) conn) >>= \case
      Left _err ->
        throwError $ err500 { errBody = BSL8.pack $ show _err }
      Right a -> pure a

databaseM :: (Connection -> IO (Either SessionError a)) -> DatabaseM a
databaseM f = DatabaseM $ ReaderT $ ExceptT . f


doSelect
    :: Serializable exprs (FromExprs exprs)
    => Query exprs
    -> DatabaseM [FromExprs exprs]
doSelect = databaseM . run . statement () . Rel8.run . select


doSelect1
    :: Serializable exprs (FromExprs exprs)
    => Query exprs
    -> DatabaseM (FromExprs exprs)
doSelect1 = databaseM . run . statement () . Rel8.run1 . select



doUpdate
    :: Serializable exprs (FromExprs exprs)
    => Update (Query exprs)
    -> DatabaseM [FromExprs exprs]
doUpdate = databaseM . run . statement () . Rel8.run . update


doUpdate_
    :: Update a
    -> DatabaseM ()
doUpdate_ = databaseM . run . statement () . Rel8.run_ . update


doInsert
    :: Serializable exprs (FromExprs exprs)
    => Insert (Query exprs)
    -> DatabaseM [FromExprs exprs]
doInsert = databaseM . run . statement () . Rel8.run . insert


doInsert1
    :: Serializable exprs (FromExprs exprs)
    => Insert (Query exprs)
    -> DatabaseM (FromExprs exprs)
doInsert1 = databaseM . run . statement () . Rel8.run1 . insert


doInsert_
    :: Insert a
    -> DatabaseM ()
doInsert_ = databaseM . run . statement () . Rel8.run_ . insert


doDelete
    :: Serializable exprs (FromExprs exprs)
    => Delete (Query exprs)
    -> DatabaseM [FromExprs exprs]
doDelete = databaseM . run . statement () . Rel8.run . delete


doDelete_ :: Delete a -> DatabaseM ()
doDelete_ = databaseM . run . statement () . Rel8.run_ . delete

