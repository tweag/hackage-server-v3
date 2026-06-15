{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hackage.Schemas.Packages where

import Hackage.Schemas.Users
import Hackage.Types.PrimaryKey
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hackage.Types
import Rel8.CreateTable

import Rel8
  ( Rel8able
  , TableSchema(..)
  , Column
  , Name
  , DBEq
  , ReadShow(..)
  , DBType
  , Rel8able
  )

data Tarball

type PkgId = PrimaryKey PackageNameRow

data PackageNameRow f = PackageNameRow
  { packageNameId :: Column f PkgId
  , packageName :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

packageNameSchema :: TableSchema (PackageNameRow Name)
packageNameSchema = TableSchema
  { name = "packages"
  , columns = PackageNameRow
      { packageNameId = "id"
      , packageName = "name"
      }
  }


packageNameTable :: DbTable PackageNameRow
packageNameTable = DbTable packageNameSchema
  [ PK packageNameId
  , AutoInc packageNameId
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
  , FK pkgId packageNameSchema packageNameId
  ]


type PkgRevId = PrimaryKey MetadataRevisionRow

data MetadataRevisionRow f = MetadataRevisionRow
  { metadataId :: Column f PkgRevId
  , metadataPkgId :: Column f PkgInfoId
  , metadataRevId :: Column f MetadataRevIx
  , metadataTime :: Column f UTCTime
  , metadataUploader :: Column f UserId
  , metadataCabalFile :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)



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
  , FK metadataPkgId pkgInfoSchema pkgInfoId
  ]


data TarballRevisionRow f = TarballRevisionRow
  { tarballPkgId :: Column f PkgInfoId
  , tarballRevId :: Column f TarballRevIx
  , tarballTime :: Column f UTCTime
  , tarballUploader :: Column f UserId
  , tarballBlobGz   :: Column f (BlobId Tarball)
  , tarballBlobNoGz :: Column f (BlobId Tarball)
  , tarballLength :: Column f Int64
  , tarballHash :: Column f SHA256Digest
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)


packageTarballRevisionsSchema :: TableSchema (TarballRevisionRow Name)
packageTarballRevisionsSchema = TableSchema
  { name = "package_tarball_revisions"
  , columns = TarballRevisionRow
      { tarballPkgId = "pkgid"
      , tarballRevId = "rev"
      , tarballTime = "upload_time"
      , tarballUploader = "revised_by"
      , tarballBlobGz   = "blob_gz"
      , tarballBlobNoGz = "blob_nogz"
      , tarballLength = "tarball_length"
      , tarballHash = "tarball_hash"
      }
  }

packageTarballRevisionsTable :: DbTable TarballRevisionRow
packageTarballRevisionsTable = DbTable packageTarballRevisionsSchema
  [ PK tarballPkgId
  , AutoInc tarballPkgId
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

data PackageMaintainerRow f = PackageMaintainerRow
  { pmId :: Column f Int64
  , pmPackageId :: Column f PkgInfoId
  , pmUserId :: Column f UserId
  , pmRole :: Column f PackageRole
  , pmAssignedTime :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

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
  ]

