module Hackage.Auth.Maintainer
  ( IsMaintainer ()
  , checkIsMaintainer
  , maintainerWitness
  , name
  ) where

import Data.The
import Hackage.Schemas.Packages
import Hackage.Types
import Hackage.Utils
import Rel8 ((==.), (&&.), each, where_, Result, lit, optional)
import Servant.Server (Handler)
import Theory.Named


-- | A proof that the 'Named' @user@ is a package maintainer. The only way to
-- get a value of this type is via 'checkIsMaintainer', after invoking 'name' on
-- a particular 'UserId' and 'PackageInfoId'.
type role IsMaintainer nominal nominal
data IsMaintainer user package = IsMaintainer (PackageMaintainerRow Result)

instance The (IsMaintainer user package) (PackageMaintainerRow Result) where
  the = maintainerWitness


-- | Get the 'UserRoleRow' that witnesses the given user is an admin.
maintainerWitness :: IsMaintainer user package -> PackageMaintainerRow Result
maintainerWitness (IsMaintainer x) = x


-- | Determine whether the named user is a maintainer of the named package.
checkIsMaintainer
    :: Connection
    -> Named user UserId
    -> Named package PkgInfoId
    -> Handler (Maybe (IsMaintainer user package))
checkIsMaintainer conn uid pkgid = do
  fmap (fmap IsMaintainer) $ doSelect1E conn $ optional $ do
    role <- each packageMaintainersSchema
    where_ $ pmUserId role    ==. lit (the uid)
         &&. pmPackageId role ==. lit (the pkgid)
    pure role

