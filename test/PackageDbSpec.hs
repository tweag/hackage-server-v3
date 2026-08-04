{-# LANGUAGE OverloadedStrings #-}

module PackageDbSpec where

import Control.Exception (throwIO, finally)
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Bool
import Data.Foldable
import Data.Map qualified as M
import Data.Pool (newPool, withResource, defaultPoolConfig)
import Data.Text.Encoding (decodeUtf8)
import Database.Postgres.Temp qualified as Temp
import Distribution.Types.PackageName
import Hackage.API.PackageDb (packageDbServer)
import Hackage.API.Type
import Hackage.Schemas.Packages
import Hackage.ServerM
import Hackage.SetupDB (setupDB)
import Hackage.Types.PrimaryKey
import Hackage.Utils
import Hasql.Connection.Setting qualified as DB
import Hasql.Connection.Setting.Connection qualified as DB
import Hasql.Session (run, sql)
import Rel8 hiding (Lift, bool, run)
import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = aroundAll withDb $ do
  serverProp "api_versions gives back what you put in" $
    \(PrintableString pkgname, pkgdepr, versions) -> do
      let pkg = mkPackageName pkgname
      pkgid <- liftDB $ doInsert1 $ Insert
        { into = packageNameSchema
        , rows = values $
            [ PackageNameRow
                { packageNameId = newPrimaryKey
                , packageName = lit pkg
                , packageDeprecated = lit pkgdepr
                }
            ]
        , onConflict = DoNothing
        , returning = Returning packageNameId
        }
      for_ (M.toList versions) $ \(version, depr) ->
        liftDB $ doInsert_ $ Insert
          { into = pkgInfoSchema
          , rows = values
              [ PkgInfoRow
                  { pkgInfoId = newPrimaryKey
                  , pkgId = lit pkgid
                  , packageVersion = lit version
                  , pkgInfoDeprecated = lit depr
                  }
              ]
          , onConflict = DoNothing
          , returning = NoReturning
          }
      vs <- pkgdb_api_versions packageDbServer pkg
      pure $ vs `shouldBe` PackageVersions (M.fromList $ do
        (version, depr) <- M.toList versions
        pure (version, bool Normal Deprecated depr)
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
    -> (a -> ServerM b)
    -> SpecWith ServerCtx
serverProp n p =
  it n $ \ctx ->
    property $ \a ->
      ioProperty $ do
        conn <- withResource (serverPool ctx) pure
        void $ run (sql "BEGIN") conn
        mb <- finally (runServerM ctx $ p a) $ liftIO $ run (sql "ROLLBACK") conn
        either throwIO pure mb

