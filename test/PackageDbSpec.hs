{-# LANGUAGE OverloadedStrings               #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module PackageDbSpec where

import Control.Exception (throwIO, finally)
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Bool
import Data.Map qualified as M
import Data.Pool (newPool, withResource, defaultPoolConfig)
import Data.Text.Encoding (decodeUtf8)
import Database.Postgres.Temp qualified as Temp
import Hackage.API.PackageDb (packageDbServer)
import Hackage.API.Type
import Hackage.ServerM
import Hackage.SetupDB (setupDB)
import Hackage.Utils
import Hasql.Connection.Setting qualified as DB
import Hasql.Connection.Setting.Connection qualified as DB
import Hasql.Session (run, sql)
import Model
import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = aroundAll withDb $ do
  serverProp "api_versions gives back what you put in" $
    \model () -> do
      (pkg, mp) <- genExistingPackage model
      pure $ do
        vs <- pkgdb_api_versions packageDbServer pkg
        pure $ vs `shouldBe` PackageVersions (M.fromList $ do
          (version, depr) <- M.toList $ mp_versions mp
          pure (version, bool Normal Deprecated $ mpi_deprecated depr)
          )


-- | For use with 'aroundAll': make a temporary postgres database and setup its
-- schema for Hackage.
withDb :: ActionWith ServerCtx -> IO ()
withDb action = do
  x <- Temp.with $ \db ->
    withConn [DB.connection $ DB.string $ decodeUtf8 $ Temp.toConnectionString db] $ \conn ->  do
      -- Make a fake pool that only has a single connection in it. This is to
      -- ensure that the entire thing runs inside of its transaction, since
      -- those are scoped to a single connection.
      pool <- newPool $ defaultPoolConfig (pure conn) (const $ pure ()) 1 100
      -- Setup the database schema
      withResource pool setupDB
      action $ ServerCtx
        { serverPool = pool
        }
  either throwIO pure x


-- | Like 'prop', but for testing properties about 'ServerM'.
serverProp
    :: (Arbitrary a, Show a, Testable b)
    => String
    -> (ModelHackage -> a -> Gen (ServerM b))
    -> SpecWith ServerCtx
serverProp n p =
  it n $ \ctx ->
    property $ \(model, a) -> do
      server <- p model a
      pure $ ioProperty $ do
        conn <- withResource (serverPool ctx) pure
        void $ run (sql "BEGIN") conn
        mb <- finally (runServerM ctx $ loadModelHackage model *> server) $ liftIO $ run (sql "ROLLBACK") conn
        either throwIO pure mb

