{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Hackage.API.Query where

import Data.Text (Text)
import Data.Functor.Contravariant
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Hackage.Schemas.Packages
import Hackage.Types
import Rel8 hiding (Lift, bool)
import Hackage.Objects


-- | A 'PackageLocator' is either a specific version of a package, or the latest
-- version. This function dispatches on that to find a specific 'PkgInfoRow' for
-- the 'PackageLocator'.
getLocator :: PackageLocator -> Query (PkgInfoRow Expr)
getLocator (Specific pid) = do
  pkg <- each packageNameSchema
  where_ $ packageName pkg ==. lit (pkgName pid)
  pkgv <- each pkgInfoSchema
  where_ $ pkgId pkgv ==. packageNameId pkg
  where_ $ packageVersion pkgv ==. lit (pkgVersion pid)
  pure pkgv
getLocator (Latest pname) =
  latestBy packageVersion $ getAllVersions $ lit pname


-- | Given a 'PackageName', get all version rows.
getAllVersions :: Expr PackageName -> Query (PkgInfoRow Expr)
getAllVersions pname = do
  pkg <- each packageNameSchema
  where_ $ packageName pkg ==. pname
  pkgv <- each pkgInfoSchema
  where_ $ pkgId pkgv ==. packageNameId pkg
  pure pkgv


-- | Refine a query by returning the row with the largest value given by a
-- projection.
latestBy :: DBOrd b => (a -> Expr b) -> Query a -> Query a
latestBy f = limit 1 . orderBy (f >$< desc)

earliestBy :: DBOrd b => (a -> Expr b) -> Query a -> Query a
earliestBy f = limit 1 . orderBy (f >$< asc)


getAllRevs :: PackageLocator -> Query (MetadataRevisionRow Expr)
getAllRevs loc = do
  pkgv <- getLocator loc
  rev <- each metadataRevisionsSchema
  where_ $ metadataPkgId rev ==. pkgInfoId pkgv
  pure rev


getLatestRev :: PackageLocator -> Query (MetadataRevisionRow Expr)
getLatestRev = onlyLatestRev . getAllRevs


-- | SQL function that converts an @'Expr' 'Version'@ into 'Text', by
-- intercalating the versions with dots.
showVersionExpr :: Expr Version -> Expr Text
showVersionExpr v = function "array_to_string" (v, lit @_ @Text ".")

-- | SQL function @starts_with@.
startsWith
    :: Expr Text
    -- ^ Haystack
    -> Expr Text
    -- ^ Needle
    -> Expr Bool
startsWith haystack needle = function "starts_with" (haystack, needle)


onlyEarliestRev
    :: Query (MetadataRevisionRow Expr)
    -> Query (MetadataRevisionRow Expr)
onlyEarliestRev = earliestBy metadataRevId

onlyLatestRev
    :: Query (MetadataRevisionRow Expr)
    -> Query (MetadataRevisionRow Expr)
onlyLatestRev = latestBy metadataRevId


getLatestTarball :: PackageLocator -> Query (TarballRevisionRow Expr)
getLatestTarball loc = do
  pkgv <- getLocator loc
  latestBy tarballTime $ do
    rev <- each packageTarballRevisionsSchema
    where_ $ tarballPkgId rev ==. pkgInfoId pkgv
    pure rev


locatorToPackageId :: PackageLocator -> Query (Expr PackageName, Expr Version)
locatorToPackageId (Specific p) = pure $ lit (pkgName p, pkgVersion p)
locatorToPackageId (Latest p) = do
  version <- getLocator $ Latest p
  pure (lit p, packageVersion version)

