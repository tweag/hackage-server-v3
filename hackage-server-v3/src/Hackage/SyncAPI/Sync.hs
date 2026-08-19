{-# LANGUAGE PackageImports #-}

module Hackage.SyncAPI.Sync where

import Control.Monad (void)
import Hackage.Import
import Hackage.Schemas.Users
import Hackage.ServerM
import Hackage.SyncAPI.Type
import Hackage.Types
import Hackage.Utils
import Hasql.Session (statement, run)
import Rel8 hiding (run)
import Rel8 qualified as Rel8
import Servant.Server.Generic (AsServerT)


syncServer :: SyncApi (AsServerT ServerM)
syncServer = SyncApi
  { sync_api_new_user = newUser
  , sync_api_new_package = newPackage
  , sync_api_revise_meta = reviseMeta
  , sync_api_revise_tarball = reviseTarball
  }


-- TODO(sandy): Better return codes for failure
newUser :: NewUserReq -> ServerM ()
newUser nur = do
  runDB $ doInsert_ $ Insert
    { into = usersSchema
    , rows = pure $
        UsersRow
          { userId = lit $ nur_userid nur
          , userName = lit $ nur_username nur
          , userStatus = lit Enabled
          }
    , onConflict = Abort
    , returning = NoReturning
    }


sqlMToDatabase
    :: Serializable exprs (FromExprs exprs)
    => SqlM (Query exprs)
    -> DatabaseM [FromExprs exprs]
sqlMToDatabase
  = databaseM . run . statement () . Rel8.run . runSqlM


-- TODO(sandy): Better return codes for failure
newPackage :: PackageId -> NewPackageReq -> ServerM ()
newPackage pid npr = do
  void $ runDB $ sqlMToDatabase $ do
    epkgid <- mkPkgIdentifier pid
    _ <-
      mkMetadataRev
        Abort
        epkgid
        (MetadataRevIx 0)
        (npr_cabalFile npr)
        (npr_uploadTime npr)
        (npr_uploader npr)
    mkTarballRev
      Abort
      epkgid
      0
      (npr_blobGz npr)
      (npr_blobNoGz npr)
      (npr_uploadTime npr)
      (npr_uploader npr)


-- TODO(sandy): Better return codes for failure
reviseMeta :: PackageId -> MetadataRevIx -> ReviseMetaReq -> ServerM ()
reviseMeta pid rev rmr =
  void $ runDB $ sqlMToDatabase $ do
    epkgid <- mkPkgIdentifier pid
    mkMetadataRev
      Abort
      epkgid
      rev
      (rmr_cabalFile rmr)
      (rmr_uploadTime rmr)
      (rmr_uploader rmr)


-- TODO(sandy): Better return codes for failure
reviseTarball :: PackageId -> TarballRevIx -> ReviseTarballReq -> ServerM ()
reviseTarball pid rev rtr =
  void $ runDB $ sqlMToDatabase $ do
    epkgid <- mkPkgIdentifier pid
    mkTarballRev
      Abort
      epkgid
      rev
      (rtr_blobGz rtr)
      (rtr_blobNoGz rtr)
      (rtr_uploadTime rtr)
      (rtr_uploader rtr)

