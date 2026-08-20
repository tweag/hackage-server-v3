module Hackage.SetupDB where

import Control.Exception (throwIO)
import Hackage.Schemas.Packages
import Hackage.Schemas.Users
import Hackage.Utils (Connection)
import Hasql.Session (run)
import Rel8 (Rel8able)
import Rel8.CreateTable


setupDB :: Connection -> IO ()
setupDB conn = do
  let mk :: Rel8able table => DbTable table -> IO ()
      mk table =
        either throwIO pure =<< flip run conn (makeTable table)

  mk usersTable
  mk packageNameTable
  mk pkgInfoTable
  mk metadataRevisionsTable
  mk packageTarballRevisionsTable
  mk tarAlreadyIndexedTable
  mk tarIndexTable
  mk pkgDeprecationTable
  mk pkgDocsTable

