{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Hackage.API.MoreAPI
  ( PackagesHtmlAPI2 (..)
  , staticHTML
  , trusteesEndpoint
  , namesStub
  ) where

import Data.Aeson hiding (Result(..))
import Data.Coerce
import Data.Functor
import Data.Hashable
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text.Arbitrary ()
import GHC.Generics (Generic)
import GHC.TypeLits
import Hackage.Schemas.Users
import Hackage.ServerM
import Hackage.Types
import Hackage.Utils
import Rel8 hiding (Lift, bool)
import Servant.API
import Servant.EDE
import Test.QuickCheck

data PackagesHtmlAPI2 mode = PackagesHtmlAPI2
  { htmlPackagesNames :: mode
      :- "packages"
      :> "names"
      :> Get '[HTML] PackageNames
  , htmlPackagesTrustees :: mode
      :- "packages"
      :> "trustees"
      :> Get '[HTML] TrusteesObject
  , htmlPackagesHelp :: mode
      :- "upload" :> Get '[HTML] UploadHelp
  , htmlPackagesUploadForm :: mode
      :- "packages"
      :> "upload"
      :> Get '[HTML] PackageUpload
  }


--------------------------------------------------------------------------------
-- /packages/names

instance HasTemplate HTML PackageNames where
  templateFor _ _ = "packages/names.html"

data PackageNames = PackageNames
  { packages :: Map Text PackageNameData
  }
  deriving stock (Show, Generic)
  deriving anyclass ToObject

instance Arbitrary PackageNames where
  arbitrary = fmap PackageNames arbitrary


data PackageNameData = PackageNameData
  { pkgDesc :: Text
  , pkgTags :: [Tag]
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

instance Arbitrary PackageNameData where
  arbitrary = PackageNameData <$> arbitrary <*> arbitrary


namesStub :: ServerM PackageNames
namesStub = pure $ PackageNames $ M.fromList
  [ ("hello", PackageNameData "from space" ["a", "b", "c"])
  , ("goodbye", PackageNameData "my dude" ["a", "b"])
  ]

--------------------------------------------------------------------------------
-- /packages/trustees
data TrusteesObject = TrusteesObject (Map UserId UserName)
  deriving stock (Eq, Show, Generic)
  deriving anyclass Hashable

instance Arbitrary TrusteesObject where
  arbitrary = fmap TrusteesObject arbitrary

instance ToJSON TrusteesObject where
  toJSON ts = Object $ toObject ts <>
    [ "title" .= id @String "Package trustees"
    , "description" .= id @String "The role of trustees is to help to curate the whole package collection. Trustees have a limited ability to edit package information, for the entire package database (as opposed to package maintainers who have full control over individual packages). Trustees can edit .cabal files, edit other package metadata and upload documentation but they cannot upload new package versions."
    ]

instance ToObject TrusteesObject where
  toObject (TrusteesObject ts) =
    [ "members" .= (M.toList ts <&> \(uid, name) ->
        object
          [ "userid" .= uid
          , "username" .= name
          ]
      )
    ]

instance HasTemplate HTML TrusteesObject where
  templateFor _ _ = "upload/trustees.html"


trusteesEndpoint :: ServerM TrusteesObject
trusteesEndpoint = do
  ts <- runDB $ doSelect $ do
    r <- each userRolesSchema
    where_ $ userRoleRole r ==. lit Trustee
    u <- activeUsers
    where_ $ userId u ==. userRoleUserId r
    pure (userRoleUserId r, userName u)

  pure $ TrusteesObject $ M.fromList ts


--------------------------------------------------------------------------------
-- | A @'StaticHTML' template@ uses the statically known type-level symbol
-- @template@ for its template. As suggested by its name, it can be used to
-- serve static HTML templates. In order to use this type, you should @newtype@
-- wrap it, and @newtype@ derive 'Eq', 'Show', 'Arbitrary', 'ToObject', and
-- @'HasTemplate' 'HTML'@. You can get a free handler for it via 'staticHTML'
--
-- The advantage of newtype-wrapping this type is that doing so prevents the
-- template names from leaking into the API contract. Furthermore, it provides
-- a forward-compatable means of making the endpoint /less/ static in the
-- future :)
type StaticHTML :: Symbol -> Type
data StaticHTML template = StaticHTML
  deriving stock (Eq, Show, Generic)
  deriving anyclass Hashable

instance Arbitrary (StaticHTML a) where
  arbitrary = pure StaticHTML

instance ToObject (StaticHTML a) where
  toObject _ = mempty

instance KnownSymbol a => HasTemplate HTML (StaticHTML a) where
  templateFor _ _ = symbolVal @a undefined

-- | Get a handler for a newtype-wrapped 'StaticHTML' value.
staticHTML :: Coercible (StaticHTML a) b => ServerM b
staticHTML = pure $ coerce StaticHTML


--------------------------------------------------------------------------------
-- /upload
newtype UploadHelp = UploadHelp (StaticHTML "upload/help.html")
  deriving newtype (Eq, Show, Hashable, Arbitrary, ToObject, HasTemplate HTML)

--------------------------------------------------------------------------------
-- /package/upload

newtype PackageUpload = PackageUpload (StaticHTML "upload/form.html")
  deriving newtype (Eq, Show, Hashable, Arbitrary, ToObject, HasTemplate HTML)

