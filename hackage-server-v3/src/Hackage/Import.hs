{-# LANGUAGE AllowAmbiguousTypes             #-}
{-# LANGUAGE OverloadedLabels                #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE PartialTypeSignatures           #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Hackage.Import where

import Control.Lens (Lens', at, view, (&), (.~))
import Control.Monad.Accum
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Generics.Labels ()
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Time (UTCTime)
import Distribution.Package qualified as Cabal
import GHC.Generics (Generically(..), Generic)
import Hackage.Schemas.Packages
import Hackage.Schemas.Users
import Hackage.Types
import Hackage.Types.PrimaryKey
import Rel8 hiding (run)
import Servant.Tarball


-- | A mapping from Haskell types to queries that cache the results of their
-- inserted IDs.
data Queries = Queries
  { names :: Map PackageName (Query (Expr PkgId))
  , versions :: Map PackageIdentifier (Query (Expr PkgInfoId))
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via Generically Queries


-- | A monad capable of generating SQL and caching 'Queries' (via the
-- 'MonadAccum' interface.)
newtype SqlM a = SqlM
  { unSqlM :: StateT Queries Statement a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadAccum Queries SqlM where
  look = SqlM get
  add = SqlM . modify' . mappend


-- | Lift a 'Statement' into 'SqlM'.
sql :: Statement a -> SqlM a
sql = SqlM . lift


-- | Run a 'SqlM' in the 'Statement' monad.
runSqlM :: SqlM a -> Statement a
runSqlM = flip evalStateT mempty . unSqlM


-- | Used for filling in 'onConflict' fields, such that a conflict on the given
-- 'Projection' will still return the desired 'Returning'.
returnKeyOnConflict
    :: (Projecting names index, _)
    => Projection names index
    -> OnConflict names
returnKeyOnConflict idx = DoUpdate $ Upsert
  { index = idx
  , predicate = Nothing
  , set = const id
  , updateWhere = \_ _ -> lit True
  }


-- | Cache the result of a 'SqlM' so that we can pull it up again quickly.
caching
  :: Ord a
  => Lens' Queries (Map a (Query (Expr b)))
  -- ^ A map inside of 'Queries' for the thing we'd like to cache.
  -> (a -> SqlM (Query (Expr b)))
  -- ^ How to generate the result on a cache miss
  -> a -> SqlM (Query (Expr b))
caching prop mk key = do
  q <- looks $ view prop
  case M.lookup key q of
    Just b -> pure b
    Nothing -> do
      b <- mk key
      add $ mempty & prop . at key .~ Just b
      pure b


-- | Insert a mostly empty user into 'usersSchema'.
mkUser :: UserId -> UserName -> SqlM (Query (Expr UserId))
mkUser uid name =
  sql $
    insert $
      Insert
        { into = usersSchema
        , rows = values @_ @[]
            [ UsersRow
                { userId = lit uid
                , userName = lit name
                , userStatus = lit Enabled
                }
            ]
        , onConflict = returnKeyOnConflict userId
        , returning = Returning userId
        }


-- | Insert a 'PackageName' into the 'packageNameSchema' table
mkPkgName :: PackageName -> SqlM (Query (Expr PkgId))
mkPkgName = caching #names $ \name -> do
  sql $
    insert $
      Insert
        { into = packageNameSchema
        , rows = values @_ @[]
            [ PackageNameRow
                { packageNameId = newPrimaryKey
                , packageName = lit name
                -- TODO(sandy): fill in deprecated status
                , packageDeprecated = lit False
                }
            ]
        , onConflict = returnKeyOnConflict packageName
        , returning = Returning packageNameId
        }


-- | Insert a 'PackageIdentifier' into the 'pkgInfoSchema' table
mkPkgIdentifier :: PackageIdentifier -> SqlM (Query (Expr PkgInfoId))
mkPkgIdentifier = caching #versions $ \pkgid -> do
  pkgname <- mkPkgName $ Cabal.packageName pkgid
  sql $ insert $
    Insert
      { into = pkgInfoSchema
      , rows = do
          pkgnameid <- pkgname
          values @_ @[]
            [ PkgInfoRow
                { pkgInfoId = newPrimaryKey
                , pkgId = pkgnameid
                , packageVersion = lit $ Cabal.packageVersion pkgid
                , pkgInfoDeprecated = lit False
                }
            ]
      , onConflict = returnKeyOnConflict $ liftA2 (,) pkgId packageVersion
      , returning = Returning pkgInfoId
      }


-- | Insert a revision into the 'metadataRevisionsSchema' table
mkMetadataRev
    :: OnConflict (MetadataRevisionRow Name)
    -> Query (Expr PkgInfoId)
    -> MetadataRevIx
    -> ByteString
    -> UTCTime
    -> UserId
    -> SqlM (Query (Expr PkgRevId))
mkMetadataRev conflict qpkgid revix cabal time uid = sql $
  insert $ Insert
    { into = metadataRevisionsSchema
    , rows = do
        pkgid <- qpkgid
        values @_ @[]
          [ MetadataRevisionRow
              { metadataId = newPrimaryKey
              , metadataPkgId = pkgid
              , metadataRevId = lit revix
              , metadataTime = lit time
              , metadataUploader = lit uid
              , metadataCabalFile = lit cabal
              }
          ]
    , onConflict = conflict
    , returning = Returning metadataId
    }


newtype TarOffset = TarOffset { unTarOffset :: Int64 }
  deriving newtype (Eq, Ord, Show, Num)


mkTarballRev
  :: OnConflict (TarballRevisionRow Name)
  -> Query (Expr PkgInfoId)
  -> TarballRevIx
  -> BlobId (Compressed Tarball)
  -> BlobId Tarball
  -> UTCTime
  -> UserId
  -> SqlM (Query (Expr TarballRevId))
mkTarballRev
    conflict
    qpkgid
    revix
    gz
    nogz
    time
    uid =
  sql $ insert $ Insert
    { into = packageTarballRevisionsSchema
    , rows = do
        pkgid <- qpkgid
        values @_ @[]
          [ TarballRevisionRow
              { tarballRevId = newPrimaryKey
              , tarballPkgId = pkgid
              , tarballRevIx = lit revix
              , tarballTime = lit time
              , tarballUploader = lit uid
              , tarballBlobGz = lit gz
              , tarballBlobNoGz = lit nogz
              }
          ]
    , onConflict = conflict
    , returning = Returning tarballRevId
    }

