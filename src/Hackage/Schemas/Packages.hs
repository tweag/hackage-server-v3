{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hackage.Schemas.Packages where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hackage.Orphans ()
import Hackage.Schemas.Users
import Hackage.Types
import Hackage.Types.PrimaryKey
import Rel8.CreateTable
import Servant.Tarball
import Data.ByteString (StrictByteString)

import Rel8
  ( Rel8able
  , TableSchema(..)
  , Column
  , Name
  , DBEq
  , ReadShow(..)
  , DBType
  , Rel8able
  , Result
  )

data Compressed a

type PkgId = PrimaryKey PackageNameRow

data PackageNameRow f = PackageNameRow
  { packageNameId :: Column f PkgId
  , packageName :: Column f PackageName
  , packageDeprecated :: Column f Bool
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (PackageNameRow Result)

packageNameSchema :: TableSchema (PackageNameRow Name)
packageNameSchema = TableSchema
  { name = "packages"
  , columns = PackageNameRow
      { packageNameId = "id"
      , packageName = "name"
      , packageDeprecated = "deprecated"
      }
  }


packageNameTable :: DbTable PackageNameRow
packageNameTable = DbTable packageNameSchema
  [ PK packageNameId
  , AutoInc packageNameId
  , Unique packageName
  ]

type PkgDeprecationKey = PrimaryKey PkgDeprecationRow

data PkgDeprecationRow f = PkgDeprecationRow
  { pkgDeprecationId :: Column f PkgDeprecationKey
  , pkgDeprecatedPkg :: Column f PkgId
  , pkgDeprecatedInFavorOf :: Column f PkgId
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (PkgDeprecationRow Result)


pkgDeprecationSchema :: TableSchema (PkgDeprecationRow Name)
pkgDeprecationSchema = TableSchema
  { name = "deprecations"
  , columns = PkgDeprecationRow
      { pkgDeprecationId = "id"
      , pkgDeprecatedPkg = "pkg"
      , pkgDeprecatedInFavorOf = "deprecated_for"
      }
  }

pkgDeprecationTable :: DbTable PkgDeprecationRow
pkgDeprecationTable = DbTable pkgDeprecationSchema
  [ PK pkgDeprecationId
  , AutoInc pkgDeprecationId
  , FK pkgDeprecatedPkg packageNameSchema packageNameId
  , FK pkgDeprecatedInFavorOf packageNameSchema packageNameId
  ]


-- | Packages metadata table

type PkgInfoId = PrimaryKey PkgInfoRow

data PkgInfoRow f = PkgInfoRow
  { pkgInfoId :: Column f PkgInfoId
  , pkgId :: Column f PkgId
  , packageVersion :: Column f Version
  , pkgInfoDeprecated :: Column f Bool
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (PkgInfoRow Result)

pkgInfoSchema :: TableSchema (PkgInfoRow Name)
pkgInfoSchema = TableSchema
  { name = "pkginfos"
  , columns = PkgInfoRow
      { pkgInfoId = "id"
      , pkgId = "package"
      , packageVersion = "version"
      , pkgInfoDeprecated = "deprecated"
      }
  }

pkgInfoTable :: DbTable PkgInfoRow
pkgInfoTable = DbTable pkgInfoSchema
  [ PK pkgInfoId
  , AutoInc pkgInfoId
  , Unique2 pkgId packageVersion
  , FK pkgId packageNameSchema packageNameId
  ]

--------------------------------------------------------------------------------

type PkgDocsId = PrimaryKey PkgDocsRow

data PkgDocsRow f = PkgDocsRow
  { pkgDocsId :: Column f PkgDocsId
  , pkgDocsPkg :: Column f PkgInfoId
  , pkgDocsTarball :: Column f (BlobId Tarball)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (PkgDocsRow Result)


pkgDocsSchema :: TableSchema (PkgDocsRow Name)
pkgDocsSchema = TableSchema
  { name = "pkg_docs"
  , columns = PkgDocsRow
      { pkgDocsId = "id"
      , pkgDocsPkg = "pkgid"
      , pkgDocsTarball = "blob"
      }
  }

pkgDocsTable :: DbTable PkgDocsRow
pkgDocsTable = DbTable pkgDocsSchema
  [ PK pkgDocsId
  , AutoInc pkgDocsId
  , Unique pkgDocsPkg
  ]


--------------------------------------------------------------------------------


type PkgRevId = PrimaryKey MetadataRevisionRow

data MetadataRevisionRow f = MetadataRevisionRow
  { metadataId :: Column f PkgRevId
  , metadataPkgId :: Column f PkgInfoId
  , metadataRevId :: Column f MetadataRevIx
  , metadataTime :: Column f UTCTime
  , metadataUploader :: Column f UserId
  , metadataCabalFile :: Column f StrictByteString
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (MetadataRevisionRow Result)



metadataRevisionsSchema :: TableSchema (MetadataRevisionRow Name)
metadataRevisionsSchema = TableSchema
  { name = "metadata_revs"
  , columns = MetadataRevisionRow
      { metadataId = "id"
      , metadataPkgId = "pkgid"
      , metadataRevId = "rev"
      , metadataTime = "time"
      , metadataUploader = "uploader"
      , metadataCabalFile = "cabal_file"
      }
  }

metadataRevisionsTable :: DbTable MetadataRevisionRow
metadataRevisionsTable = DbTable metadataRevisionsSchema
  [ PK metadataId
  , AutoInc metadataId
  , Unique2 metadataPkgId metadataRevId
  , FK metadataPkgId pkgInfoSchema pkgInfoId
  ]

type TarballRevId = PrimaryKey TarballRevisionRow

data TarballRevisionRow f = TarballRevisionRow
  { tarballRevId :: Column f TarballRevId
  , tarballPkgId :: Column f PkgInfoId
  , tarballRevIx :: Column f TarballRevIx
  , tarballTime :: Column f UTCTime
  , tarballUploader :: Column f UserId
  , tarballBlobGz   :: Column f (BlobId (Compressed Tarball))
  , tarballBlobNoGz :: Column f (BlobId Tarball)
  , tarballGzLength :: Column f Int64
  , tarballGzHash :: Column f SHA256Digest
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (TarballRevisionRow Result)


packageTarballRevisionsSchema :: TableSchema (TarballRevisionRow Name)
packageTarballRevisionsSchema = TableSchema
  { name = "package_tarball_revisions"
  , columns = TarballRevisionRow
      { tarballRevId = "id"
      , tarballPkgId = "pkgid"
      , tarballRevIx = "rev"
      , tarballTime = "upload_time"
      , tarballUploader = "revised_by"
      , tarballBlobGz   = "blob_gz"
      , tarballBlobNoGz = "blob_nogz"
      , tarballGzLength = "tarball_length"
      , tarballGzHash = "tarball_hash"
      }
  }

packageTarballRevisionsTable :: DbTable TarballRevisionRow
packageTarballRevisionsTable = DbTable packageTarballRevisionsSchema
  [ PK tarballRevId
  , AutoInc tarballRevId
  , Unique2 tarballPkgId tarballRevId
  , FK tarballPkgId pkgInfoSchema pkgInfoId
  ]

-- | Package maintainers
-- PRIMARY KEY (synthetic): pmId
-- The (package_name, user_id, role) triple should be unique to prevent
-- duplicate role assignments.
data PackageRole = Maintainer
  deriving stock (Show, Read, Eq, Ord, Generic, Enum, Bounded)
  deriving (DBType) via ReadShow PackageRole
  deriving anyclass (DBEq)

type PackageMaintainerId = PrimaryKey PackageMaintainerRow

data PackageMaintainerRow f = PackageMaintainerRow
  { pmId :: Column f PackageMaintainerId
  , pmPackageId :: Column f PkgInfoId
  , pmUserId :: Column f UserId
  , pmRole :: Column f PackageRole
  , pmAssignedTime :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (PackageMaintainerRow Result)

packageMaintainersSchema :: TableSchema (PackageMaintainerRow Name)
packageMaintainersSchema = TableSchema
  { name = "package_maintainers"
  , columns = PackageMaintainerRow
      { pmId = "package_maintainer_id"
      , pmPackageId = "package_id"
      , pmUserId = "user_id"
      , pmRole = "role"
      , pmAssignedTime = "assigned_time"
      }
  }

packageMaintainerTable :: DbTable PackageMaintainerRow
packageMaintainerTable = DbTable packageMaintainersSchema
  [ PK pmId
  , AutoInc pmId
  , FK pmPackageId pkgInfoSchema pkgInfoId
  , FK pmUserId usersSchema userId
  ]


type TagId = PrimaryKey TagRow

-- | Package tags for categorization
--
-- PRIMARY KEY (synthetic): ptId
-- Each (package_name, tag) pair is stored as a separate row. The pair
-- should be unique to prevent assigning the same tag twice to a package.
data TagRow f = TagRow
  { tagId :: Column f TagId
  , tagTag :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (TagRow Result)


tagsSchema :: TableSchema (TagRow Name)
tagsSchema = TableSchema
  { name = "tags"
  , columns = TagRow
      { tagId = "id"
      , tagTag = "tag"
      }
  }

tagTable :: DbTable TagRow
tagTable = DbTable tagsSchema
  [ PK tagId
  , AutoInc tagId
  ]

-- | Package tags for categorization
--
-- PRIMARY KEY (synthetic): ptId
-- Each (package_name, tag) pair is stored as a separate row. The pair
-- should be unique to prevent assigning the same tag twice to a package.
type PackageTagId = PrimaryKey PackageTagRow

data PackageTagRow f = PackageTagRow
  { ptId :: Column f PackageTagId
  , ptPackageRevId :: Column f PkgRevId
  , ptTagId :: Column f TagId
  , ptAssignedTime :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (PackageTagRow Result)

packageTagsSchema :: TableSchema (PackageTagRow Name)
packageTagsSchema = TableSchema
  { name = "package_tags"
  , columns = PackageTagRow
      { ptId = "package_tag_id"
      , ptPackageRevId = "package_rev_id"
      , ptTagId = "tag"
      , ptAssignedTime = "assigned_time"
      }
  }

packageTagTable :: DbTable PackageTagRow
packageTagTable = DbTable packageTagsSchema
  [ PK ptId
  , AutoInc ptId
  , FK ptPackageRevId metadataRevisionsSchema metadataId
  ]

type TarIndexId = PrimaryKey TarIndexRow

data TarIndexRow f = TarIndexRow
  { tarIndexId :: Column f TarIndexId
  , tarIndexBlob :: Column f (BlobId Tarball)
  , tarIndexPath :: Column f Text
  , tarIndexOffset :: Column f Int64
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (TarIndexRow Result)

tarIndexSchema :: TableSchema (TarIndexRow Name)
tarIndexSchema = TableSchema
  { name = "tarindex"
  , columns = TarIndexRow
      { tarIndexId = "id"
      , tarIndexBlob = "blob"
      , tarIndexPath = "path"
      , tarIndexOffset = "offset"
      }
  }


tarIndexTable :: DbTable TarIndexRow
tarIndexTable = DbTable tarIndexSchema
  [ PK tarIndexId
  , AutoInc tarIndexId
  , Unique2 tarIndexBlob tarIndexPath
  , TextPatternOps tarIndexPath
  ]

