{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hackage.Schemas.Packages where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hackage.Types

import Rel8
  ( Rel8able
  , TableSchema(..)
  , Column
  , Name
  , DBEq
  , DBOrd
  , ReadShow(..)
  , DBType
  , Rel8able
  )

newtype PkgInfoId = PkgInfoId { getPkgInfoId :: Int64 }
  deriving newtype (Eq, Ord, Show, Read, DBEq, DBOrd, DBType)

-- | Packages metadata table
--
-- PRIMARY KEY (natural): packageId
data PkgInfoRow f = PkgInfoRow
  { packageId :: Column f PkgInfoId
  , packageName :: Column f PackageName
  , packageVersion :: Column f Version
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

pkgInfoSchema :: TableSchema (PkgInfoRow Name)
pkgInfoSchema = TableSchema
  { name = "pkginfos"
  , columns = PkgInfoRow
      { packageId = "id"
      , packageName = "name"
      , packageVersion = "version"
      }
  }


data MetadataRevisionRow f = MetadataRevisionRow
  { metadataPkgId :: Column f PkgInfoId
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
      { metadataPkgId = "pkgid"
      , metadataRevId = "rev"
      , metadataTime = "time"
      , metadataUploader = "uploader"
      , metadataCabalFile = "cabal_file"
      }
  }



data TarballRevisionRow f = TarballRevisionRow
  { tarballPkgId :: Column f PkgInfoId
  , tarballRevId :: Column f TarballRevIx
  , tarballTime :: Column f UTCTime
  , tarballUploader :: Column f UserId
  , tarballBlobGz   :: Column f BlobId
  , tarballBlobNoGz :: Column f BlobId
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

-- | Package versions and tarballs
-- PRIMARY KEY (synthetic): pvId
-- The (package_id, version) pair should be unique to prevent duplicate versions.
data PackageVersionRow f = PackageVersionRow
  { pvId :: Column f Int64
  , pvPackageId :: Column f Int32
  , pvVersion :: Column f Version
  , pvUploadedBy :: Column f UserId
  , pvUploadTime :: Column f UTCTime
  , pvTarballBlob :: Column f BlobId
  , pvCabalBlob :: Column f BlobId
  , pvIsCandidate :: Column f Bool
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

packageVersionsSchema :: TableSchema (PackageVersionRow Name)
packageVersionsSchema = TableSchema
  { name = "package_versions"
  , columns = PackageVersionRow
      { pvId = "package_version_id"
      , pvPackageId = "package_id"
      , pvVersion = "version"
      , pvUploadedBy = "uploaded_by"
      , pvUploadTime = "upload_time"
      , pvTarballBlob = "tarball_blob"
      , pvCabalBlob = "cabal_blob"
      , pvIsCandidate = "is_candidate"
      }
  }

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

-- | Package tags for categorization
--
-- PRIMARY KEY (synthetic): ptId
-- Each (package_name, tag) pair is stored as a separate row. The pair
-- should be unique to prevent assigning the same tag twice to a package.
data PackageTagRow f = PackageTagRow
  { ptId :: Column f Int64
  , ptPackageName :: Column f PackageName
  , ptTag :: Column f Text
  , ptAssignedTime :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

packageTagsSchema :: TableSchema (PackageTagRow Name)
packageTagsSchema = TableSchema
  { name = "package_tags"
  , columns = PackageTagRow
      { ptId = "package_tag_id"
      , ptPackageName = "package_name"
      , ptTag = "tag"
      , ptAssignedTime = "assigned_time"
      }
  }

-- | Tag aliases for tag normalization
--
-- PRIMARY KEY (synthetic): taId
-- Each (tag, alias) pair is stored as a separate row.
data TagAliasRow f = TagAliasRow
  { taId    :: Column f Int64
  , taTag   :: Column f Text
  , taAlias :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

tagAliasesSchema :: TableSchema (TagAliasRow Name)
tagAliasesSchema = TableSchema
  { name = "tag_aliases"
  , columns = TagAliasRow
      { taId    = "tag_alias_id"
      , taTag   = "tag"
      , taAlias = "alias"
      }
  }

-- | Deprecated package versions.
--
-- PRIMARY KEY (synthetic): depId
-- A package can have many deprecated versions, so each deprecated version
-- is a separate row. The (package_name, version) pair should be unique.
data DeprecatedVersionRow f = DeprecatedVersionRow
  { depId :: Column f Int64
  , depPackageName :: Column f PackageName
  , depVersion :: Column f Version
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deprecatedVersionsSchema :: TableSchema (DeprecatedVersionRow Name)
deprecatedVersionsSchema = TableSchema
  { name = "deprecated_versions"
  , columns = DeprecatedVersionRow
      { depId = "deprecated_version_id"
      , depPackageName = "package_name"
      , depVersion = "version"
      }
  }

-- | Package documentation storage
--
-- PRIMARY KEY (synthetic): docId
data DocumentationRow f = DocumentationRow
  { docId :: Column f Int64
  , docPackageId :: Column f Int32
  , docBlobId :: Column f BlobId
  , docStoredTime :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

documentationSchema :: TableSchema (DocumentationRow Name)
documentationSchema = TableSchema
  { name = "documentation"
  , columns = DocumentationRow
      { docId = "documentation_id"
      , docPackageId = "package_id"
      , docBlobId = "blob_id"
      , docStoredTime = "stored_time"
      }
  }


data TarIndexRow f = TarIndexRow
  { tarIndexId :: Column f Int64
  , tarIndexBlob :: Column f BlobId
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

