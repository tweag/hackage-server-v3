{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hackage.Schemas.Packages where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hackage.Orphans ()
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
  , Rel8able
  , Result
  )


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
      }
  }

packageTarballRevisionsTable :: DbTable TarballRevisionRow
packageTarballRevisionsTable = DbTable packageTarballRevisionsSchema
  [ PK tarballRevId
  , AutoInc tarballRevId
  , Unique2 tarballPkgId tarballRevId
  , FK tarballPkgId pkgInfoSchema pkgInfoId
  ]


type TarAlreadyIndexedId = PrimaryKey TarIndexRow

data TarAlreadyIndexedRow f = TarAlreadyIndexedRow
  { tarAlreadyIndexedId :: Column f TarAlreadyIndexedId
  , tarAlreadyIndexedBlob :: Column f (BlobId Tarball)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (TarAlreadyIndexedRow Result)

{-# WARNING in "x-dontuse"
    tarAlreadyIndexedSchema
    "It's an error to use this schema directly, since it gets loaded lazily. Instead route your logic through 'Hackage.TarIndex.indexingTarIndices'."
    #-}
tarAlreadyIndexedSchema :: TableSchema (TarAlreadyIndexedRow Name)
tarAlreadyIndexedSchema = TableSchema
  { name = "already_indexed"
  , columns = TarAlreadyIndexedRow
      { tarAlreadyIndexedId = "id"
      , tarAlreadyIndexedBlob = "blob"
      }
  }


tarAlreadyIndexedTable :: DbTable TarAlreadyIndexedRow
tarAlreadyIndexedTable = DbTable tarAlreadyIndexedSchema
  [ PK tarAlreadyIndexedId
  , Unique tarAlreadyIndexedBlob
  ]

type TarIndexId = PrimaryKey TarIndexRow

data TarIndexRow f = TarIndexRow
  { tarIndexId :: Column f TarIndexId
  , tarIndexKey :: Column f TarAlreadyIndexedId
  , tarIndexPath :: Column f Text
  , tarIndexOffset :: Column f Int64
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (TarIndexRow Result)

{-# WARNING in "x-dontuse"
    tarIndexSchema
    "It's an error to use this schema directly, since it gets loaded lazily. Instead route your logic through 'Hackage.TarIndex.indexingTarIndices'."
    #-}
tarIndexSchema :: TableSchema (TarIndexRow Name)
tarIndexSchema = TableSchema
  { name = "tarindex"
  , columns = TarIndexRow
      { tarIndexId = "id"
      , tarIndexKey = "indexid"
      , tarIndexPath = "path"
      , tarIndexOffset = "offset"
      }
  }


tarIndexTable :: DbTable TarIndexRow
tarIndexTable = DbTable tarIndexSchema
  [ PK tarIndexId
  , AutoInc tarIndexId
  , FK tarIndexKey tarAlreadyIndexedSchema tarAlreadyIndexedId
  , Unique2 tarIndexKey tarIndexPath
  ]

