module Hackage.Auth.Admin
  ( IsAdmin
  , checkIsAdmin
  , IsTrustee
  , checkIsTrustee
  , IsUploader
  , checkIsUploader
  , roleWitness
  , name
  ) where

import Data.Kind (Type)
import Data.The
import Hackage.Schemas.Users
import Hackage.Types
import Hackage.Utils
import Rel8 ((==.), (&&.), each, where_, Result, lit, optional)
import Theory.Named


-- | A proof that the 'Named' @user@ is the given @userRole@. The only way to get a
-- value of this type is via 'checkIsAdmin' or 'checkIsTrustee', after invoking
-- 'name' on a particular 'UserId'.
type role HasRole nominal nominal
type HasRole :: UserRole -> Type -> Type
data HasRole userRole user = HasRole (UserRoleRow Result)

instance The (HasRole userRole user) (UserRoleRow Result) where
  the = roleWitness


-- | A proof that the named user is an admin.
type IsAdmin = HasRole 'Admin


-- | A proof that the named user is a trustee.
type IsTrustee = HasRole 'Trustee


-- | A proof that the named user is an uploader.
type IsUploader = HasRole 'Uploader


-- | Get the 'UserRoleRow' that witnesses the given user is an admin.
roleWitness :: HasRole userRole user -> UserRoleRow Result
roleWitness (HasRole x) = x


-- | Common implementation of 'checkIsAdmin' and 'checkIsTrustee'
checkIsImpl :: UserRole -> Named user UserId -> ServerM (Maybe (HasRole userRole user))
checkIsImpl urole uid = do
  fmap (fmap HasRole) $ liftDB $ doSelect1 $ optional $ do
    role <- each userRolesSchema
    where_ $ userRoleRole role ==. lit urole
         &&. userRoleUserId role ==. lit (the uid)
    pure role


-- | Attempt to summon up a proof that the given user is an admin.
checkIsAdmin :: Named user UserId -> ServerM (Maybe (IsAdmin user))
checkIsAdmin = checkIsImpl Admin


-- | Attempt to summon up a proof that the given user is a trustee.
checkIsTrustee :: Named user UserId -> ServerM (Maybe (IsTrustee user))
checkIsTrustee = checkIsImpl Trustee


-- | Attempt to summon up a proof that the given user is an uploader.
checkIsUploader :: Named user UserId -> ServerM (Maybe (IsUploader user))
checkIsUploader = checkIsImpl Uploader

