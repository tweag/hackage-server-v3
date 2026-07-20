module SetupDB where

import Hackage.Utils (Connection)
import Hackage.Schemas.Packages
import Hackage.Schemas.Users
import Hasql.Session (run)
import Rel8.CreateTable
import Rel8 (Rel8able)


main :: Connection -> IO ()
main conn = do
  let mk :: Rel8able table => DbTable table -> IO ()
      mk table = print =<< flip run conn (makeTable table)

  mk usersTable
  mk userRolesTable
  mk userAuthTokensTable
  mk packageNameTable
  mk pkgInfoTable
  mk metadataRevisionsTable
  mk packageTarballRevisionsTable
  mk packageMaintainerTable
  mk tagTable
  mk packageTagTable
  mk tarIndexTable

