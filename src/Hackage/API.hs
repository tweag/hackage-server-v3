{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Hackage.API where

import Servant.HackageCombinators
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.HTML.Blaze
import Hackage.Types

type TODO = ()
type IndexTarball = ()

type CSV = ()
type RSS = ()
type XML = ()
type TAR = ()
type Tarball = ()

data IndexTarballAPI mode = IndexTarballAPI
    { indexTarball :: mode :- "01-index.tar" :> Get '[OctetStream] IndexTarball
    , indexTarballGz :: mode :- "01-index.tar.gz" :> Get '[OctetStream] IndexTarball
    }

-- `/admin`              | GET    | html    | admin-frontend
-- `/admin/account/:uid` | GET    | html    | admin-frontend
-- `/admin/accounts`     | GET    | html    | admin-frontend
-- `/admin/deauth`       | GET    | *       | core
-- `/admin/log.:format`  | GET    | html    | admin-actions-log
-- `/admin/resets`       | GET    | html    | admin-frontend
-- `/admin/signups`      | GET    | html    | admin-frontend
data AdminAPI mode = AdminApi
    { adminPage :: mode :- Get '[HTML] ()
    , adminUser :: mode :- "admin" :> "account" :> Capture "uid" UserId :> Get '[HTML] ()
    , adminAccounts :: mode :- "admin" :> "accounts" :> Get '[HTML] ()
    , adminDeauth :: mode :- "admin" :> "deauth" :> Get '[HTML, JSON] ()
    , adminLog :: mode :- "admin" :> "log.html" :> Get '[HTML] ()
    , adminResets :: mode :- "admin" :> "resets" :> Get '[HTML] ()
    , adminSignups :: mode :- "admin" :> "signups" :> Get '[HTML] ()
    }
    deriving stock (Generic)

-- `/distro/:distro`                                      | PUT    | *       | distro
-- `/distro/:distro`                                      | DELETE | *       | distro
-- `/distro/:distro/package/:package.:format`             | GET    | txt     | distro
-- `/distro/:distro/package/:package.:format`             | PUT    | *       | distro
-- `/distro/:distro/package/:package.:format`             | DELETE | *       | distro
-- `/distro/:distro/packages.:format`                     | GET    | csv     | distro
-- `/distro/:distro/packages.:format`                     | GET    | txt     | distro
-- `/distro/:distro/packages.:format`                     | PUT    | csv     | distro
-- `/distro/:package/maintainers/.:format`                | GET    | json    | distro
-- `/distro/:package/maintainers/user/:username.:format`  | PUT    | *       | distro
-- `/distro/:package/maintainers/user/:username.:format`  | DELETE | *       | distro
-- `/distros/.:format` | GET    | txt     | distro
-- `/distros/.:format` | POST   | *       | distro
data DistroAPI mode = DistroAPI
    { distroPut :: mode :- "distro" :> IsAdmin :> Capture "distro" DistroName :> Put '[TODO] ()
    , distroDel :: mode :- "distro" :> IsAdmin :> Capture "distro" DistroName :> Delete '[TODO] ()
    , distroPkgGet :: mode :- "distro" :> Capture "distro" DistroName :> "package" :> Capture "package" (WithFormat PackageName "txt") :> Get '[PlainText] ()
    , distroPkgPut :: mode :- "distro" :> Capture "distro" DistroName :> "package" :> Capture "package" (WithFormat PackageName "txt") :> Put '[PlainText] ()
    , distroPkgDel :: mode :- "distro" :> Capture "distro" DistroName :> "package" :> Capture "package" (WithFormat PackageName "txt") :> Delete '[PlainText] ()
    , distroPkgsGetCsv :: mode :- "distro" :> Capture "distro" DistroName :> "packages.csv" :> Get '[CSV] ()
    , distroPkgsPutCsv :: mode :- "distro" :> Capture "distro" DistroName :> "packages.csv" :> Put '[CSV] ()
    , distroPkgsGetTxt :: mode :- "distro" :> Capture "distro" DistroName :> "packages.txt" :> Get '[PlainText] ()
    , distroPkgMaintainersGet :: mode :- "distro" :> Capture "package" PackageName :> "maintainers" :> "user" :> Get '[JSON] ()
    , distroPkgMaintainersPut :: mode :- "distro" :> Capture "package" PackageName :> "maintainers" :> "user" :> Capture "username" (WithAnyFormat UserName) :> Put '[] ()
    , distroPkgMaintainersDel :: mode :- "distro" :> Capture "package" PackageName :> "maintainers" :> "user" :> Capture "username" (WithAnyFormat UserName) :> Delete '[] ()
    , distrosGet :: mode :- "distros" :> IsAdmin :> Get '[PlainText] ()
    , distrosPost :: mode :- "distros" :> IsAdmin :> Post '[] ()
    }
    deriving stock (Generic)

data AllAPI mode = AllAPI
    { adminApi :: mode :- NamedRoutes AdminAPI
    }
    deriving stock (Generic)

type API = NamedRoutes AllAPI

api :: Proxy API
api = Proxy


data IsAdmin

-- `/package/:package/candidate/:cabal.cabal`             | GET    | cabal   | candidates               |
-- `/package/:package/candidate/:tarball.tar.gz`          | GET    | tarball | candidates               |
-- `/package/:package/candidate/changelog.:format`        | GET    | html    | candidates               |
-- `/package/:package/candidate/changelog.:format`        | GET    | txt     | candidates               |
-- `/package/:package/candidate/src/...`                  | GET    | *       | candidates               |
-- `/package/:package/candidates/.:format`                | GET    | json    | candidates               |
-- `/packages/candidates/.:format`                        | GET    | json    | candidates               |
-- `/packages/candidates/.:format`                        | POST   | txt     | candidates               |
data CandidatesAPI mode = CandidatesAPI
    { candidateCabalGet :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> Capture "cabal" (WithFormat PackageName "cabal") :> Get '[PlainText] ()
    , candidateTarballGet :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> Capture "tarball" (WithFormat PackageName "tar.gz") :> Get '[Tarball] ()
    , candidateChangelogHtml :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "changelog.html" :> Get '[HTML] ()
    , candidateChangelogTxt :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "changelog.txt" :> Get '[PlainText] ()
    , candidateSrcGet :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "src" :> Raw
    , packageCandidatesGet :: mode :- "package" :> Capture "package" PackageName :> "candidates" :> Get '[JSON] ()
    , packagesCandidatesGet :: mode :- "packages" :> "candidates" :> Get '[JSON] ()
    , packagesCandidatesPost :: mode :- "packages" :> "candidates" :> Post '[PlainText] ()
    }
    deriving stock (Generic)

-- `/package/`                                            | GET    | *       | core                     |
-- `/package/:package/:cabal.cabal`                       | GET    | cabal   | core                     |
-- `/package/:package/:tarball.tar.gz`                    | GET    | tarball | core                     |
-- `/package/:package/revision/:revision.:format`         | GET    | cabal   | core                     |
-- `/package/:package/revisions/.:format`                 | GET    | json    | core                     |
-- `/packages/.:format`                                   | GET    | json    | core                     |
-- `/packages/deauth`                                     | GET    | *       | core                     |
-- `/packages/index.tar.gz`                               | GET    | tarball | core                     |
-- `/user/:user/deauth`                                   | GET    | *       | core                     |
data CoreAPI mode = CoreAPI
    { packageIndex :: mode :- "package" :> Get '[TODO] ()
    , packageCabalGet :: mode :- "package" :> Capture "package" PackageName :> Capture "cabal" (WithFormat PackageName "cabal") :> Get '[PlainText] ()
    , packageTarballGet :: mode :- "package" :> Capture "package" PackageName :> Capture "tarball" (WithFormat PackageName "tar.gz") :> Get '[OctetStream] ()
    , packageRevisionGet :: mode :- "package" :> Capture "package" PackageName :> "revision" :> Capture "revision" (WithFormat Revision "cabal") :> Get '[PlainText] ()
    , packageRevisionsGet :: mode :- "package" :> Capture "package" PackageName :> "revisions" :> Get '[JSON] ()
    , packagesGet :: mode :- "packages" :> Get '[JSON] ()
    , packagesDeauth :: mode :- "packages" :> "deauth" :> Get '[TODO] ()
    , packagesIndexTarball :: mode :- "packages" :> "index.tar.gz" :> Get '[Tarball] ()
    , userDeauth :: mode :- "user" :> Capture "user" UserName :> "deauth" :> Get '[TODO] ()
    }
    deriving stock (Generic)

-- `/package/:package/candidate/docs.:format`             | GET    | tar     | documentation-candidates |
-- `/package/:package/candidate/docs.:format`             | PUT    | tar     | documentation-candidates |
-- `/package/:package/candidate/docs.:format`             | DELETE | *       | documentation-candidates |
-- `/package/:package/candidate/docs/...`                 | GET    | *       | documentation-candidates |
-- `/packages/candidates/docs.:format`                    | GET    | json    | documentation-candidates |
data DocumentationCandidatesAPI mode = DocumentationCandidatesAPI
    { candidateDocsGet :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "docs.tar" :> Get '[TAR] ()
    , candidateDocsPut :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "docs.tar" :> Put '[TAR] ()
    , candidateDocsDel :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "docs" :> Delete '[TODO] ()
    , candidateDocsBrowse :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "docs" :> Raw
    , packagesCandidatesDocsGet :: mode :- "packages" :> "candidates" :> "docs" :> Get '[JSON] ()
    }
    deriving stock (Generic)

-- `/package/:package/docs.:format`                       | GET    | tar     | documentation-core       |
-- `/package/:package/docs.:format`                       | PUT    | tar     | documentation-core       |
-- `/package/:package/docs.:format`                       | DELETE | *       | documentation-core       |
-- `/package/:package/docs/...`                           | GET    | *       | documentation-core       |
-- `/packages/docs.:format`                               | GET    | json    | documentation-core       |
data DocumentationCoreAPI mode = DocumentationCoreAPI
    { packageDocsGet :: mode :- "package" :> Capture "package" PackageName :> "docs.tar" :> Get '[TAR] ()
    , packageDocsPut :: mode :- "package" :> Capture "package" PackageName :> "docs.tar" :> Put '[TAR] ()
    , packageDocsDel :: mode :- "package" :> Capture "package" PackageName :> "docs" :> Delete '[TODO] ()
    , packageDocsBrowse :: mode :- "package" :> Capture "package" PackageName :> "docs" :> Raw
    , packagesDocsGet :: mode :- "packages" :> "docs" :> Get '[JSON] ()
    }
    deriving stock (Generic)

-- `/packages/downloads.:format`                          | GET    | csv     | download                 |
-- `/packages/downloads.:format`                          | PUT    | csv     | download                 |
-- `/packages/top.:format`                                | GET    | json    | download                 |
data DownloadAPI mode = DownloadAPI
    { packagesDownloadsGet :: mode :- "packages" :> "downloads.csv" :> Get '[CSV] ()
    , packagesDownloadsPut :: mode :- "packages" :> "downloads.csv" :> Put '[CSV] ()
    , packagesTopGet :: mode :- "packages" :> "top.json" :> Get '[JSON] ()
    }
    deriving stock (Generic)

-- `/package/:package/:cabal.cabal/edit`                  | GET    | html    | edit-cabal-files         |
-- `/package/:package/:cabal.cabal/edit`                  | POST   | html    | edit-cabal-files         |
data EditCabalFilesAPI mode = EditCabalFilesAPI
    { packageCabalEditGet :: mode :- "package" :> Capture "package" PackageName :> Capture "cabal" (WithFormat PackageName "cabal") :> "edit" :> Get '[HTML] ()
    , packageCabalEditPost :: mode :- "package" :> Capture "package" PackageName :> Capture "cabal" (WithFormat PackageName "cabal") :> "edit" :> Post '[HTML] ()
    }
    deriving stock (Generic)

-- `/user/:username/endorse`                              | GET    | html    | endorse                  |
-- `/user/:username/endorse`                              | POST   | html    | endorse                  |
data EndorseAPI mode = EndorseAPI
    { userEndorseGet :: mode :- "user" :> Capture "username" UserName :> "endorse" :> Get '[HTML] ()
    , userEndorsePost :: mode :- "user" :> Capture "username" UserName :> "endorse" :> Post '[HTML] ()
    }
    deriving stock (Generic)

-- | `/packages/hoogle.tar.gz`                              | GET    | tarball | hoogle-data              |
data HoogleDataAPI mode = HoogleDataAPI
    { packagesHoogleTarball :: mode :- "packages" :> "hoogle.tar.gz" :> Get '[Tarball] ()
    }
    deriving stock (Generic)

-- `/package/:package.:format`                            | GET    | html    | html                     |
-- `/package/:package/analytics-pixels.:format`           | GET    | html    | html                     |
-- `/package/:package/analytics-pixels.:format`           | POST   | html    | html                     |
-- `/package/:package/analytics-pixels.:format`           | DELETE | html    | html                     |
-- `/package/:package/candidate.:format`                  | GET    | html    | html                     |
-- `/package/:package/candidate.:format`                  | PUT    | html    | html                     |
-- `/package/:package/candidate.:format`                  | DELETE | html    | html                     |
-- `/package/:package/candidate/delete.:format`           | GET    | html    | html                     |
-- `/package/:package/candidate/delete.:format`           | POST   | html    | html                     |
-- `/package/:package/candidate/dependencies`             | GET    | html    | html                     |
-- `/package/:package/candidate/docs.:format`             | PUT    | html    | html                     |
-- `/package/:package/candidate/docs.:format`             | DELETE | html    | html                     |
-- `/package/:package/candidate/maintain`                 | GET    | html    | html                     |
-- `/package/:package/candidate/maintain/docs`            | GET    | html    | html                     |
-- `/package/:package/candidate/publish.:format`          | GET    | html    | html                     |
-- `/package/:package/candidate/publish.:format`          | POST   | html    | html                     |
-- `/package/:package/candidate/upload`                   | GET    | html    | html                     |
-- `/package/:package/candidates/.:format`                | GET    | html    | html                     |
-- `/package/:package/candidates/.:format`                | POST   | *       | html                     |
-- `/package/:package/candidates/delete.:format`          | GET    | html    | html                     |
-- `/package/:package/candidates/delete.:format`          | POST   | html    | html                     |
-- `/packages/candidates/.:format`                        | GET    | html    | html                     |
-- `/packages/candidates/.:format`                        | POST   | html    | html                     |
-- `/packages/candidates/upload`                          | GET    | html    | html                     |
data CandidatesHtmlAPI mode = CandidatesHtmlAPI
    { htmlCandidateGet :: mode :- "package" :> Capture "package" PackageName :> "candidate.html" :> Get '[HTML] ()
    , htmlCandidatePut :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> Put '[HTML] ()
    , htmlCandidateDel :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> Delete '[HTML] ()
    , htmlCandidateDeleteGet :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "delete.html" :> Get '[HTML] ()
    , htmlCandidateDeletePost :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "delete" :> Post '[HTML] ()
    , htmlCandidateDependencies :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "dependencies" :> Get '[HTML] ()
    , htmlCandidateDocsPut :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "docs" :> Put '[HTML] ()
    , htmlCandidateDocsDel :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "docs" :> Delete '[HTML] ()
    , htmlCandidateMaintain :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "maintain" :> Get '[HTML] ()
    , htmlCandidateMaintainDocs :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "maintain" :> "docs" :> Get '[HTML] ()
    , htmlCandidatePublishGet :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "publish.html" :> Get '[HTML] ()
    , htmlCandidatePublishPost :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "publish" :> Post '[HTML] ()
    , htmlCandidateUpload :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "upload" :> Get '[HTML] ()
    , htmlPackageCandidatesGet :: mode :- "package" :> Capture "package" PackageName :> "candidates" :> Get '[HTML] ()
    , htmlPackageCandidatesPost :: mode :- "package" :> Capture "package" PackageName :> "candidates" :> Post '[TODO] ()
    , htmlPackageCandidatesDeleteGet :: mode :- "package" :> Capture "package" PackageName :> "candidates" :> "delete.html" :> Get '[HTML] ()
    , htmlPackageCandidatesDeletePost :: mode :- "package" :> Capture "package" PackageName :> "candidates" :> "delete" :> Post '[HTML] ()
    , htmlPackagesCandidatesGet :: mode :- "packages" :> "candidates" :> Get '[HTML] ()
    , htmlPackagesCandidatesPost :: mode :- "packages" :> "candidates" :> Post '[HTML] ()
    , htmlPackagesCandidatesUpload :: mode :- "packages" :> "candidates" :> "upload" :> Get '[HTML] ()
    }
    deriving stock (Generic)

-- `/package/:package.:format`                            | GET    | html    | html                     |
-- `/package/:package/analytics-pixels.:format`           | GET    | html    | html                     |
-- `/package/:package/analytics-pixels.:format`           | POST   | html    | html                     |
-- `/package/:package/analytics-pixels.:format`           | DELETE | html    | html                     |
-- `/package/:package/dependencies`                       | GET    | html    | html                     |
-- `/package/:package/deprecated.:format`                 | GET    | html    | html                     |
-- `/package/:package/deprecated.:format`                 | PUT    | html    | html                     |
-- `/package/:package/deprecated/edit`                    | GET    | html    | html                     |
-- `/package/:package/distro-monitor.:format`             | GET    | html    | html                     |
-- `/package/:package/docs.:format`                       | PUT    | html    | html                     |
-- `/package/:package/docs.:format`                       | DELETE | html    | html                     |
-- `/package/:package/maintain`                           | GET    | html    | html                     |
-- `/package/:package/maintain/docs`                      | GET    | html    | html                     |
-- `/package/:package/preferred.:format`                  | GET    | html    | html                     |
-- `/package/:package/preferred.:format`                  | PUT    | html    | html                     |
-- `/package/:package/preferred/edit`                     | GET    | html    | html                     |
-- `/package/:package/reports/.:format`                   | GET    | html    | html                     |
-- `/package/:package/reports/:id.:format`                | GET    | html    | html                     |
-- `/package/:package/reports/testsEnabled/`              | GET    | html    | html                     |
-- `/package/:package/reverse.:format`                    | GET    | html    | html                     |
-- `/package/:package/reverse/flat.:format`               | GET    | html    | html                     |
-- `/package/:package/reverse/old.:format`                | GET    | html    | html                     |
-- `/package/:package/reverse/verbose.:format`            | GET    | html    | html                     |
-- `/package/:package/revisions/.:format`                 | GET    | html    | html                     |
-- `/package/:package/tags.:format`                       | GET    | html    | html                     |
-- `/package/:package/tags.:format`                       | PUT    | html    | html                     |
-- `/package/:package/tags/edit`                          | GET    | html    | html                     |
data PackageHtmlAPI mode = PackageHtmlAPI
    { htmlPackageGet :: mode :- "package" :> Capture "package" (WithFormat PackageName "html") :> Get '[HTML] ()
    , htmlPackageAnalyticsGet :: mode :- "package" :> Capture "package" PackageName :> "analytics-pixels.html" :> Get '[HTML] ()
    , htmlPackageAnalyticsPost :: mode :- "package" :> Capture "package" PackageName :> "analytics-pixels" :> Post '[HTML] ()
    , htmlPackageAnalyticsDel :: mode :- "package" :> Capture "package" PackageName :> "analytics-pixels" :> Delete '[HTML] ()
    , htmlPackageDependencies :: mode :- "package" :> Capture "package" PackageName :> "dependencies" :> Get '[HTML] ()
    , htmlPackageDeprecatedGet :: mode :- "package" :> Capture "package" PackageName :> "deprecated.html" :> Get '[HTML] ()
    , htmlPackageDeprecatedPut :: mode :- "package" :> Capture "package" PackageName :> "deprecated" :> Put '[HTML] ()
    , htmlPackageDeprecatedEdit :: mode :- "package" :> Capture "package" PackageName :> "deprecated" :> "edit" :> Get '[HTML] ()
    , htmlPackageDistroMonitor :: mode :- "package" :> Capture "package" PackageName :> "distro-monitor.html" :> Get '[HTML] ()
    , htmlPackageDocsPut :: mode :- "package" :> Capture "package" PackageName :> "docs" :> Put '[HTML] ()
    , htmlPackageDocsDel :: mode :- "package" :> Capture "package" PackageName :> "docs" :> Delete '[HTML] ()
    , htmlPackageMaintain :: mode :- "package" :> Capture "package" PackageName :> "maintain" :> Get '[HTML] ()
    , htmlPackageMaintainDocs :: mode :- "package" :> Capture "package" PackageName :> "maintain" :> "docs" :> Get '[HTML] ()
    , htmlPackagePreferredGet :: mode :- "package" :> Capture "package" PackageName :> "preferred.html" :> Get '[HTML] ()
    , htmlPackagePreferredPut :: mode :- "package" :> Capture "package" PackageName :> "preferred" :> Put '[HTML] ()
    , htmlPackagePreferredEdit :: mode :- "package" :> Capture "package" PackageName :> "preferred" :> "edit" :> Get '[HTML] ()
    , htmlPackageReportsGet :: mode :- "package" :> Capture "package" PackageName :> "reports" :> Get '[HTML] ()
    , htmlPackageReportIdGet :: mode :- "package" :> Capture "package" PackageName :> "reports" :> Capture "id" (WithFormat ReportId "html") :> Get '[HTML] ()
    , htmlPackageReportsTestsEnabled :: mode :- "package" :> Capture "package" PackageName :> "reports" :> "testsEnabled" :> Get '[HTML] ()
    , htmlPackageReverseGet :: mode :- "package" :> Capture "package" PackageName :> "reverse.html" :> Get '[HTML] ()
    , htmlPackageReverseFlatGet :: mode :- "package" :> Capture "package" PackageName :> "reverse" :> "flat.html" :> Get '[HTML] ()
    , htmlPackageReverseOldGet :: mode :- "package" :> Capture "package" PackageName :> "reverse" :> "old.html" :> Get '[HTML] ()
    , htmlPackageReverseVerboseGet :: mode :- "package" :> Capture "package" PackageName :> "reverse" :> "verbose.html" :> Get '[HTML] ()
    , htmlPackageRevisionsGet :: mode :- "package" :> Capture "package" PackageName :> "revisions" :> Get '[HTML] ()
    , htmlPackageTagsGet :: mode :- "package" :> Capture "package" PackageName :> "tags.html" :> Get '[HTML] ()
    , htmlPackageTagsPut :: mode :- "package" :> Capture "package" PackageName :> "tags" :> Put '[HTML] ()
    , htmlPackageTagsEdit :: mode :- "package" :> Capture "package" PackageName :> "tags" :> "edit" :> Get '[HTML] ()
    }
    deriving stock (Generic)

-- `/package/:package/maintainers/.:format`               | GET    | html    | html                     |
-- `/package/:package/maintainers/.:format`               | POST   | html    | html                     |
-- `/package/:package/maintainers/edit`                   | GET    | html    | html                     |
-- `/package/:package/maintainers/user/:username.:format` | DELETE | html    | html                     |
-- `/packages/mirrorers/.:format`                         | GET    | html    | html                     |
-- `/packages/mirrorers/.:format`                         | POST   | html    | html                     |
-- `/packages/mirrorers/edit`                             | GET    | html    | html                     |
-- `/packages/mirrorers/user/:username.:format`           | DELETE | html    | html                     |
-- `/packages/trustees/.:format`                          | GET    | html    | html                     |
-- `/packages/trustees/.:format`                          | POST   | html    | html                     |
-- `/packages/trustees/edit`                              | GET    | html    | html                     |
-- `/packages/trustees/user/:username.:format`            | DELETE | html    | html                     |
-- `/packages/upload`                                     | GET    | html    | html                     |
-- `/packages/uploaders/.:format`                         | GET    | html    | html                     |
-- `/packages/uploaders/.:format`                         | POST   | html    | html                     |
-- `/packages/uploaders/edit`                             | GET    | html    | html                     |
-- `/packages/uploaders/user/:username.:format`           | DELETE | html    | html                     |
data MaintainersHtmlAPI mode = MaintainersHtmlAPI
    { htmlPackageMaintainersGet :: mode :- "package" :> Capture "package" PackageName :> "maintainers" :> Get '[HTML] ()
    , htmlPackageMaintainersPost :: mode :- "package" :> Capture "package" PackageName :> "maintainers" :> Post '[HTML] ()
    , htmlPackageMaintainersEdit :: mode :- "package" :> Capture "package" PackageName :> "maintainers" :> "edit" :> Get '[HTML] ()
    , htmlPackageMaintainerDel :: mode :- "package" :> Capture "package" PackageName :> "maintainers" :> "user" :> Capture "username" (WithFormat UserName "html") :> Delete '[HTML] ()
    , htmlMirrorersGet :: mode :- "packages" :> "mirrorers" :> Get '[HTML] ()
    , htmlMirrorersPost :: mode :- "packages" :> "mirrorers" :> Post '[HTML] ()
    , htmlMirrorersEdit :: mode :- "packages" :> "mirrorers" :> "edit" :> Get '[HTML] ()
    , htmlMirrorerUserDel :: mode :- "packages" :> "mirrorers" :> "user" :> Capture "username" (WithFormat UserName "html") :> Delete '[HTML] ()
    , htmlTrusteesGet :: mode :- "packages" :> "trustees" :> Get '[HTML] ()
    , htmlTrusteesPost :: mode :- "packages" :> "trustees" :> Post '[HTML] ()
    , htmlTrusteesEdit :: mode :- "packages" :> "trustees" :> "edit" :> Get '[HTML] ()
    , htmlTrusteeUserDel :: mode :- "packages" :> "trustees" :> "user" :> Capture "username" (WithFormat UserName "html") :> Delete '[HTML] ()
    , htmlPackagesUpload :: mode :- "packages" :> "upload" :> Get '[HTML] ()
    , htmlUploadersGet :: mode :- "packages" :> "uploaders" :> Get '[HTML] ()
    , htmlUploadersPost :: mode :- "packages" :> "uploaders" :> Post '[HTML] ()
    , htmlUploadersEdit :: mode :- "packages" :> "uploaders" :> "edit" :> Get '[HTML] ()
    , htmlUploaderUserDel :: mode :- "packages" :> "uploaders" :> "user" :> Capture "username" (WithFormat UserName "html") :> Delete '[HTML] ()
    }
    deriving stock (Generic)

-- `/packages/.:format`                                   | GET    | html    | html                     |
-- `/packages/.:format`                                   | POST   | html    | html                     |
-- `/packages/browse`                                     | GET    | html    | html                     |
-- `/packages/deprecated.:format`                         | GET    | html    | html                     |
-- `/packages/graph`                                      | GET    | html    | html                     |
-- `/packages/graph.json`                                 | GET    | json    | html                     |
-- `/packages/names`                                      | GET    | html    | html                     |
-- `/packages/preferred.:format`                          | GET    | html    | html                     |
-- `/packages/recent.:format`                             | GET    | html    | html                     |
-- `/packages/recent.:format`                             | GET    | rss     | html                     |
-- `/packages/recent/revisions.:format`                   | GET    | html    | html                     |
-- `/packages/recent/revisions.:format`                   | GET    | rss     | html                     |
-- `/packages/reverse.:format`                            | GET    | html    | html                     |
-- `/packages/search.:format`                             | GET    | html    | html                     |
-- `/packages/tag/:tag.:format`                           | GET    | html    | html                     |
-- `/packages/tag/:tag/alias`                             | PUT    | html    | html                     |
-- `/packages/tag/:tag/alias/edit`                        | GET    | html    | html                     |
-- `/packages/tags/.:format`                              | GET    | html    | html                     |
-- `/packages/top.:format`                                | GET    | html    | html                     |
data PackagesHtmlAPI mode = PackagesHtmlAPI
    { htmlPackagesGet :: mode :- "packages" :> Get '[HTML] ()
    , htmlPackagesPost :: mode :- "packages" :> Post '[HTML] ()
    , htmlPackagesBrowse :: mode :- "packages" :> "browse" :> Get '[HTML] ()
    , htmlPackagesDeprecated :: mode :- "packages" :> "deprecated.html" :> Get '[HTML] ()
    , htmlPackagesGraph :: mode :- "packages" :> "graph" :> Get '[HTML] ()
    , htmlPackagesGraphJson :: mode :- "packages" :> "graph.json" :> Get '[JSON] ()
    , htmlPackagesNames :: mode :- "packages" :> "names" :> Get '[HTML] ()
    , htmlPackagesPreferred :: mode :- "packages" :> "preferred.html" :> Get '[HTML] ()
    , htmlPackagesRecentHtml :: mode :- "packages" :> "recent.html" :> Get '[HTML] ()
    , htmlPackagesRecentRss :: mode :- "packages" :> "recent.rss" :> Get '[RSS] ()
    , htmlPackagesRecentRevisionsHtml :: mode :- "packages" :> "recent" :> "revisions.html" :> Get '[HTML] ()
    , htmlPackagesRecentRevisionsRss :: mode :- "packages" :> "recent" :> "revisions.rss" :> Get '[RSS] ()
    , htmlPackagesReverse :: mode :- "packages" :> "reverse.html" :> Get '[HTML] ()
    , htmlPackagesSearch :: mode :- "packages" :> "search.html" :> Get '[HTML] ()
    , htmlPackagesTagGet :: mode :- "packages" :> "tag" :> Capture "tag" (WithFormat Tag "html") :> Get '[HTML] ()
    , htmlPackagesTagAliasPut :: mode :- "packages" :> "tag" :> Capture "tag" Tag :> "alias" :> Put '[HTML] ()
    , htmlPackagesTagAliasEdit :: mode :- "packages" :> "tag" :> Capture "tag" Tag :> "alias" :> "edit" :> Get '[HTML] ()
    , htmlPackagesTagsGet :: mode :- "packages" :> "tags" :> Get '[HTML] ()
    , htmlPackagesTop :: mode :- "packages" :> "top.html" :> Get '[HTML] ()
    }
    deriving stock (Generic)

-- `/user/:username.:format`                              | GET    | html    | html                     |
-- `/user/:username/analytics-pixels.:format`             | GET    | html    | html                     |
-- `/user/:username/analytics-pixels.:format`             | POST   | html    | html                     |
-- `/user/:username/analytics-pixels.:format`             | DELETE | html    | html                     |
-- `/user/:username/password.:format`                     | GET    | html    | html                     |
-- `/user/:username/password.:format`                     | PUT    | html    | html                     |
-- `/users/.:format`                                      | GET    | html    | html                     |
-- `/users/.:format`                                      | POST   | html    | html                     |
-- `/users/admins/.:format`                               | GET    | html    | html                     |
-- `/users/admins/.:format`                               | POST   | html    | html                     |
-- `/users/admins/edit`                                   | GET    | html    | html                     |
-- `/users/admins/user/:username.:format`                 | DELETE | html    | html                     |
-- `/users/register`                                      | GET    | html    | html                     |
data UsersHtmlAPI mode = UsersHtmlAPI
    { htmlUserGet :: mode :- "user" :> Capture "username" (WithFormat UserName "html") :> Get '[HTML] ()
    , htmlUserAnalyticsGet :: mode :- "user" :> Capture "username" UserName :> "analytics-pixels.html" :> Get '[HTML] ()
    , htmlUserAnalyticsPost :: mode :- "user" :> Capture "username" UserName :> "analytics-pixels" :> Post '[HTML] ()
    , htmlUserAnalyticsDel :: mode :- "user" :> Capture "username" UserName :> "analytics-pixels" :> Delete '[HTML] ()
    , htmlUserPasswordGet :: mode :- "user" :> Capture "username" UserName :> "password.html" :> Get '[HTML] ()
    , htmlUserPasswordPut :: mode :- "user" :> Capture "username" UserName :> "password" :> Put '[HTML] ()
    , htmlUsersGet :: mode :- "users" :> Get '[HTML] ()
    , htmlUsersPost :: mode :- "users" :> Post '[HTML] ()
    , htmlUsersAdminsGet :: mode :- "users" :> "admins" :> Get '[HTML] ()
    , htmlUsersAdminsPost :: mode :- "users" :> "admins" :> Post '[HTML] ()
    , htmlUsersAdminsEdit :: mode :- "users" :> "admins" :> "edit" :> Get '[HTML] ()
    , htmlUsersAdminUserDel :: mode :- "users" :> "admins" :> "user" :> Capture "username" (WithFormat UserName "html") :> Delete '[HTML] ()
    , htmlUsersRegister :: mode :- "users" :> "register" :> Get '[HTML] ()
    }
    deriving stock (Generic)

-- `/package/:package/:cabal.cabal`                       | PUT    | *       | mirror                   |
-- `/package/:package/:tarball.tar.gz`                    | PUT    | *       | mirror                   |
-- `/package/:package/upload-time`                        | GET    | *       | mirror                   |
-- `/package/:package/upload-time`                        | PUT    | *       | mirror                   |
-- `/package/:package/uploader`                           | GET    | *       | mirror                   |
-- `/package/:package/uploader`                           | PUT    | *       | mirror                   |
-- `/packages/mirrorers/.:format`                         | GET    | json    | mirror                   |
-- `/packages/mirrorers/user/:username.:format`           | PUT    | *       | mirror                   |
-- `/packages/mirrorers/user/:username.:format`           | DELETE | *       | mirror                   |
data MirrorAPI mode = MirrorAPI
    { mirrorPackageCabalPut :: mode :- "package" :> Capture "package" PackageName :> Capture "cabal" (WithFormat PackageName "cabal") :> Put '[TODO] ()
    , mirrorPackageTarballPut :: mode :- "package" :> Capture "package" PackageName :> Capture "tarball" (WithFormat PackageName "tar.gz") :> Put '[TODO] ()
    , mirrorPackageUploadTimeGet :: mode :- "package" :> Capture "package" PackageName :> "upload-time" :> Get '[TODO] ()
    , mirrorPackageUploadTimePut :: mode :- "package" :> Capture "package" PackageName :> "upload-time" :> Put '[TODO] ()
    , mirrorPackageUploaderGet :: mode :- "package" :> Capture "package" PackageName :> "uploader" :> Get '[TODO] ()
    , mirrorPackageUploaderPut :: mode :- "package" :> Capture "package" PackageName :> "uploader" :> Put '[TODO] ()
    , mirrorersGet :: mode :- "packages" :> "mirrorers" :> Get '[JSON] ()
    , mirrorerUserPut :: mode :- "packages" :> "mirrorers" :> "user" :> Capture "username" (WithAnyFormat UserName) :> Put '[TODO] ()
    , mirrorerUserDel :: mode :- "packages" :> "mirrorers" :> "user" :> Capture "username" (WithAnyFormat UserName) :> Delete '[TODO] ()
    }
    deriving stock (Generic)

-- | `/package/:package.rss`                                | GET    | rss     | package feed             |
data PackageFeedAPI mode = PackageFeedAPI
    { packageRss :: mode :- "package" :> Capture "package" (WithFormat PackageName "rss") :> Get '[RSS] ()
    }
    deriving stock (Generic)

-- `/package/:package/changelog.:format`                  | GET    | html    | package-contents         |
-- `/package/:package/changelog.:format`                  | GET    | txt     | package-contents         |
-- `/package/:package/readme.:format`                     | GET    | html    | package-contents         |
-- `/package/:package/readme.:format`                     | GET    | txt     | package-contents         |
-- `/package/:package/src/...`                            | GET    | *       | package-contents         |
data PackageContentsAPI mode = PackageContentsAPI
    { packageChangelogHtml :: mode :- "package" :> Capture "package" PackageName :> "changelog.html" :> Get '[HTML] ()
    , packageChangelogTxt :: mode :- "package" :> Capture "package" PackageName :> "changelog.txt" :> Get '[PlainText] ()
    , packageReadmeHtml :: mode :- "package" :> Capture "package" PackageName :> "readme.html" :> Get '[HTML] ()
    , packageReadmeTxt :: mode :- "package" :> Capture "package" PackageName :> "readme.txt" :> Get '[PlainText] ()
    , packageSrc :: mode :- "package" :> Capture "package" PackageName :> "src" :> Raw
    }
    deriving stock (Generic)

-- `/package/:package.:format`                            | GET    | json    | package-info-json        |
-- `/package/:package/revision/:revision.:format`         | GET    | json    | package-info-json        |
data PackageInfoJsonAPI mode = PackageInfoJsonAPI
    { packageInfoJson :: mode :- "package" :> Capture "package" (WithFormat PackageName "json") :> Get '[JSON] ()
    , packageRevisionInfoJson :: mode :- "package" :> Capture "package" PackageName :> "revision" :> Capture "revision" (WithFormat Revision "json") :> Get '[JSON] ()
    }
    deriving stock (Generic)

-- `/package/:package/candidate/reports/.:format`         | GET    | txt     | reports-candidates       |
-- `/package/:package/candidate/reports/.:format`         | POST   | *       | reports-candidates       |
-- `/package/:package/candidate/reports/.:format`         | PUT    | json    | reports-candidates       |
-- `/package/:package/candidate/reports/:id.:format`      | GET    | txt     | reports-candidates       |
-- `/package/:package/candidate/reports/:id.:format`      | DELETE | *       | reports-candidates       |
-- `/package/:package/candidate/reports/:id/log`          | GET    | txt     | reports-candidates       |
-- `/package/:package/candidate/reports/:id/log`          | PUT    | *       | reports-candidates       |
-- `/package/:package/candidate/reports/:id/log`          | DELETE | *       | reports-candidates       |
-- `/package/:package/candidate/reports/:id/test`         | GET    | txt     | reports-candidates       |
-- `/package/:package/candidate/reports/:id/test`         | PUT    | *       | reports-candidates       |
-- `/package/:package/candidate/reports/:id/test`         | DELETE | *       | reports-candidates       |
-- `/package/:package/candidate/reports/reset/`           | GET    | *       | reports-candidates       |
-- `/package/:package/candidate/reports/testsEnabled/`    | GET    | json    | reports-candidates       |
-- `/package/:package/candidate/reports/testsEnabled/`    | POST   | *       | reports-candidates       |
data ReportsCandidatesAPI mode = ReportsCandidatesAPI
    { candidateReportsGet :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "reports" :> Get '[PlainText] ()
    , candidateReportsPost :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "reports" :> Post '[TODO] ()
    , candidateReportsPut :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "reports" :> Put '[JSON] ()
    , candidateReportIdGet :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "reports" :> Capture "id" (WithFormat ReportId "txt") :> Get '[PlainText] ()
    , candidateReportIdDel :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "reports" :> Capture "id" (WithAnyFormat ReportId) :> Delete '[TODO] ()
    , candidateReportLogGet :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "reports" :> Capture "id" ReportId :> "log" :> Get '[PlainText] ()
    , candidateReportLogPut :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "reports" :> Capture "id" ReportId :> "log" :> Put '[TODO] ()
    , candidateReportLogDel :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "reports" :> Capture "id" ReportId :> "log" :> Delete '[TODO] ()
    , candidateReportTestGet :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "reports" :> Capture "id" ReportId :> "test" :> Get '[PlainText] ()
    , candidateReportTestPut :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "reports" :> Capture "id" ReportId :> "test" :> Put '[TODO] ()
    , candidateReportTestDel :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "reports" :> Capture "id" ReportId :> "test" :> Delete '[TODO] ()
    , candidateReportsReset :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "reports" :> "reset" :> Get '[TODO] ()
    , candidateReportsTestsEnabled :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "reports" :> "testsEnabled" :> Get '[JSON] ()
    , candidateReportsTestsEnabledPost :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "reports" :> "testsEnabled" :> Post '[TODO] ()
    }
    deriving stock (Generic)

-- `/package/:package/reports/.:format`                   | GET    | txt     | reports-core             |
-- `/package/:package/reports/.:format`                   | POST   | *       | reports-core             |
-- `/package/:package/reports/.:format`                   | PUT    | json    | reports-core             |
-- `/package/:package/reports/:id.:format`                | GET    | txt     | reports-core             |
-- `/package/:package/reports/:id.:format`                | DELETE | *       | reports-core             |
-- `/package/:package/reports/:id/log`                    | GET    | txt     | reports-core             |
-- `/package/:package/reports/:id/log`                    | PUT    | *       | reports-core             |
-- `/package/:package/reports/:id/log`                    | DELETE | *       | reports-core             |
-- `/package/:package/reports/:id/test`                   | GET    | txt     | reports-core             |
-- `/package/:package/reports/:id/test`                   | PUT    | *       | reports-core             |
-- `/package/:package/reports/:id/test`                   | DELETE | *       | reports-core             |
-- `/package/:package/reports/reset/`                     | GET    | *       | reports-core             |
-- `/package/:package/reports/testsEnabled/`              | GET    | json    | reports-core             |
-- `/package/:package/reports/testsEnabled/`              | POST   | *       | reports-core             |
data ReportsCoreAPI mode = ReportsCoreAPI
    { reportsGet :: mode :- "package" :> Capture "package" PackageName :> "reports" :> Get '[PlainText] ()
    , reportsPost :: mode :- "package" :> Capture "package" PackageName :> "reports" :> Post '[TODO] ()
    , reportsPut :: mode :- "package" :> Capture "package" PackageName :> "reports" :> Put '[JSON] ()
    , reportIdGet :: mode :- "package" :> Capture "package" PackageName :> "reports" :> Capture "id" (WithFormat ReportId "txt") :> Get '[PlainText] ()
    , reportIdDel :: mode :- "package" :> Capture "package" PackageName :> "reports" :> Capture "id" (WithAnyFormat ReportId) :> Delete '[TODO] ()
    , reportLogGet :: mode :- "package" :> Capture "package" PackageName :> "reports" :> Capture "id" ReportId :> "log" :> Get '[PlainText] ()
    , reportLogPut :: mode :- "package" :> Capture "package" PackageName :> "reports" :> Capture "id" ReportId :> "log" :> Put '[TODO] ()
    , reportLogDel :: mode :- "package" :> Capture "package" PackageName :> "reports" :> Capture "id" ReportId :> "log" :> Delete '[TODO] ()
    , reportTestGet :: mode :- "package" :> Capture "package" PackageName :> "reports" :> Capture "id" ReportId :> "test" :> Get '[PlainText] ()
    , reportTestPut :: mode :- "package" :> Capture "package" PackageName :> "reports" :> Capture "id" ReportId :> "test" :> Put '[TODO] ()
    , reportTestDel :: mode :- "package" :> Capture "package" PackageName :> "reports" :> Capture "id" ReportId :> "test" :> Delete '[TODO] ()
    , reportsReset :: mode :- "package" :> Capture "package" PackageName :> "reports" :> "reset" :> Get '[TODO] ()
    , reportsTestsEnabled :: mode :- "package" :> Capture "package" PackageName :> "reports" :> "testsEnabled" :> Get '[JSON] ()
    , reportsTestsEnabledPost :: mode :- "package" :> Capture "package" PackageName :> "reports" :> "testsEnabled" :> Post '[TODO] ()
    }
    deriving stock (Generic)

-- `/packages/opensearch.xml`                             | GET    | xml     | search                   |
-- `/packages/search.:format`                             | GET    | json    | search                   |
data SearchAPI mode = SearchAPI
    { packagesOpensearch :: mode :- "packages" :> "opensearch.xml" :> Get '[XML] ()
    , packagesSearchJson :: mode :- "packages" :> "search.json" :> Get '[JSON] ()
    }
    deriving stock (Generic)

-- `/packages/noscript-search`                            | GET    | html    | search/browse backend    |
-- `/packages/noscript-search`                            | POST   | html    | search/browse backend    |
-- `/packages/search`                                     | POST   | json    | search/browse backend    |
data SearchBrowseAPI mode = SearchBrowseAPI
    { packagesNoscriptSearchGet :: mode :- "packages" :> "noscript-search" :> Get '[HTML] ()
    , packagesNoscriptSearchPost :: mode :- "packages" :> "noscript-search" :> Post '[HTML] ()
    , packagesSearchPost :: mode :- "packages" :> "search" :> Post '[JSON] ()
    }
    deriving stock (Generic)

-- `/mirrors.json`                                        | GET    | json    | security                 |
-- `/root.json`                                           | GET    | json    | security                 |
-- `/snapshot.json`                                       | GET    | json    | security                 |
-- `/timestamp.json`                                      | GET    | json    | security                 |
data SecurityAPI mode = SecurityAPI
    { mirrorsJson :: mode :- "mirrors.json" :> Get '[JSON] ()
    , rootJson :: mode :- "root.json" :> Get '[JSON] ()
    , snapshotJson :: mode :- "snapshot.json" :> Get '[JSON] ()
    , timestampJson :: mode :- "timestamp.json" :> Get '[JSON] ()
    }
    deriving stock (Generic)

-- | `/server-status/memory.:format`                        | GET    | html    | serverapi                |
data ServerApiAPI mode = ServerApiAPI
    { serverStatusMemory :: mode :- "server-status" :> "memory.html" :> Get '[HTML] ()
    }
    deriving stock (Generic)

-- `/sitemap/:filename`                                   | GET    | xml     | sitemap                  |
-- `/sitemap_index.xml`                                   | GET    | xml     | sitemap                  |
data SitemapAPI mode = SitemapAPI
    { sitemapFile :: mode :- "sitemap" :> Capture "filename" Text :> Get '[XML] ()
    , sitemapIndex :: mode :- "sitemap_index.xml" :> Get '[XML] ()
    }
    deriving stock (Generic)

-- `/accounts`                                            | GET    | *       | static-files             |
-- `/hackageErrorPage`                                    | GET    | *       | static-files             |
-- `/index`                                               | GET    | *       | static-files             |
-- `/static/...`                                          | GET    | *       | static-files             |
-- `/upload`                                              | GET    | *       | static-files             |
data StaticFilesAPI mode = StaticFilesAPI
    { staticAccounts :: mode :- "accounts" :> Get '[HTML] ()
    , staticHackageError :: mode :- "hackageErrorPage" :> Get '[HTML] ()
    -- TODO(sandy): we also need an empty segment for the root
    , staticIndex :: mode :- "index" :> Get '[HTML] ()
    , staticFiles :: mode :- "static" :> Raw
    , staticUpload :: mode :- "upload" :> Get '[HTML] ()
    }
    deriving stock (Generic)

-- `/server-status/tarindices.:format`                    | GET    | json    | tarIndexCache            |
-- `/server-status/tarindices.:format`                    | DELETE | *       | tarIndexCache            |
data TarIndexCacheAPI mode = TarIndexCacheAPI
    { tarIndicesGet :: mode :- "server-status" :> "tarindices.json" :> Get '[JSON] ()
    , tarIndicesDel :: mode :- "server-status" :> "tarindices" :> Delete '[TODO] ()
    }
    deriving stock (Generic)

-- `/package/:package/maintainers/.:format`               | GET    | json    | upload                   |
-- `/package/:package/maintainers/user/:username.:format` | PUT    | *       | upload                   |
-- `/package/:package/maintainers/user/:username.:format` | DELETE | *       | upload                   |
-- `/packages/.:format`                                   | POST   | txt     | upload                   |
-- `/packages/trustees/.:format`                          | GET    | json    | upload                   |
-- `/packages/trustees/user/:username.:format`            | PUT    | *       | upload                   |
-- `/packages/trustees/user/:username.:format`            | DELETE | *       | upload                   |
-- `/packages/uploaders/.:format`                         | GET    | json    | upload                   |
-- `/packages/uploaders/user/:username.:format`           | PUT    | *       | upload                   |
-- `/packages/uploaders/user/:username.:format`           | DELETE | *       | upload                   |
data UploadAPI mode = UploadAPI
    { packageMaintainersGet :: mode :- "package" :> Capture "package" PackageName :> "maintainers" :> Get '[JSON] ()
    , packageMaintainerPut :: mode :- "package" :> Capture "package" PackageName :> "maintainers" :> "user" :> Capture "username" (WithAnyFormat UserName) :> Put '[TODO] ()
    , packageMaintainerDel :: mode :- "package" :> Capture "package" PackageName :> "maintainers" :> "user" :> Capture "username" (WithAnyFormat UserName) :> Delete '[TODO] ()
    , packagesPost :: mode :- "packages" :> Post '[PlainText] ()
    , trusteesGet :: mode :- "packages" :> "trustees" :> Get '[JSON] ()
    , trusteeUserPut :: mode :- "packages" :> "trustees" :> "user" :> Capture "username" (WithAnyFormat UserName) :> Put '[TODO] ()
    , trusteeUserDel :: mode :- "packages" :> "trustees" :> "user" :> Capture "username" (WithAnyFormat UserName) :> Delete '[TODO] ()
    , uploadersGet :: mode :- "packages" :> "uploaders" :> Get '[JSON] ()
    , uploaderUserPut :: mode :- "packages" :> "uploaders" :> "user" :> Capture "username" (WithAnyFormat UserName) :> Put '[TODO] ()
    , uploaderUserDel :: mode :- "packages" :> "uploaders" :> "user" :> Capture "username" (WithAnyFormat UserName) :> Delete '[TODO] ()
    }
    deriving stock (Generic)

-- `/user/:username/admin-info.:format`                   | GET    | json    | user-details             |
-- `/user/:username/admin-info.:format`                   | PUT    | json    | user-details             |
-- `/user/:username/admin-info.:format`                   | DELETE | *       | user-details             |
-- `/user/:username/name-contact.:format`                 | GET    | html    | user-details             |
-- `/user/:username/name-contact.:format`                 | GET    | json    | user-details             |
-- `/user/:username/name-contact.:format`                 | PUT    | json    | user-details             |
-- `/user/:username/name-contact.:format`                 | DELETE | *       | user-details             |
data UserDetailsAPI mode = UserDetailsAPI
    { userAdminInfoGet :: mode :- "user" :> Capture "username" UserName :> "admin-info.json" :> Get '[JSON] ()
    , userAdminInfoPut :: mode :- "user" :> Capture "username" UserName :> "admin-info.json" :> Put '[JSON] ()
    , userAdminInfoDel :: mode :- "user" :> Capture "username" UserName :> "admin-info" :> Delete '[TODO] ()
    , userNameContactGetHtml :: mode :- "user" :> Capture "username" UserName :> "name-contact.html" :> Get '[HTML] ()
    , userNameContactGetJson :: mode :- "user" :> Capture "username" UserName :> "name-contact.json" :> Get '[JSON] ()
    , userNameContactPut :: mode :- "user" :> Capture "username" UserName :> "name-contact.json" :> Put '[JSON] ()
    , userNameContactDel :: mode :- "user" :> Capture "username" UserName :> "name-contact" :> Delete '[TODO] ()
    }
    deriving stock (Generic)

-- `/user/:username/notify.:format`                       | GET    | html    | user-notify              |
-- `/user/:username/notify.:format`                       | GET    | json    | user-notify              |
-- `/user/:username/notify.:format`                       | PUT    | json    | user-notify              |
data UserNotifyAPI mode = UserNotifyAPI
    { userNotifyGetHtml :: mode :- "user" :> Capture "username" UserName :> "notify.html" :> Get '[HTML] ()
    , userNotifyGetJson :: mode :- "user" :> Capture "username" UserName :> "notify.json" :> Get '[JSON] ()
    , userNotifyPut :: mode :- "user" :> Capture "username" UserName :> "notify.json" :> Put '[JSON] ()
    }
    deriving stock (Generic)

-- `/users/password-reset`                                | GET    | *       | user-signup-reset        |
-- `/users/password-reset`                                | POST   | *       | user-signup-reset        |
-- `/users/password-reset/:nonce`                         | GET    | *       | user-signup-reset        |
-- `/users/password-reset/:nonce`                         | POST   | *       | user-signup-reset        |
-- `/users/register-request`                              | GET    | *       | user-signup-reset        |
-- `/users/register-request`                              | POST   | *       | user-signup-reset        |
-- `/users/register-request/:nonce`                       | GET    | *       | user-signup-reset        |
-- `/users/register-request/:nonce`                       | POST   | *       | user-signup-reset        |
-- `/users/register/captcha`                              | GET    | json    | user-signup-reset        |
data UserSignupResetAPI mode = UserSignupResetAPI
    { passwordResetGet :: mode :- "users" :> "password-reset" :> Get '[TODO] ()
    , passwordResetPost :: mode :- "users" :> "password-reset" :> Post '[TODO] ()
    , passwordResetNonceGet :: mode :- "users" :> "password-reset" :> Capture "nonce" Nonce :> Get '[TODO] ()
    , passwordResetNoncePost :: mode :- "users" :> "password-reset" :> Capture "nonce" Nonce :> Post '[TODO] ()
    , registerRequestGet :: mode :- "users" :> "register-request" :> Get '[TODO] ()
    , registerRequestPost :: mode :- "users" :> "register-request" :> Post '[TODO] ()
    , registerRequestNonceGet :: mode :- "users" :> "register-request" :> Capture "nonce" Nonce :> Get '[TODO] ()
    , registerRequestNoncePost :: mode :- "users" :> "register-request" :> Capture "nonce" Nonce :> Post '[TODO] ()
    , registerCaptcha :: mode :- "users" :> "register" :> "captcha" :> Get '[JSON] ()
    }
    deriving stock (Generic)

-- `/user/:username.:format`                              | GET    | json    | users                    |
-- `/user/:username.:format`                              | PUT    | *       | users                    |
-- `/user/:username.:format`                              | DELETE | *       | users                    |
-- `/user/:username/enabled.:format`                      | GET    | json    | users                    |
-- `/user/:username/enabled.:format`                      | PUT    | json    | users                    |
-- `/user/:username/manage.:format`                       | GET    | *       | users                    |
-- `/user/:username/manage.:format`                       | POST   | *       | users                    |
-- `/users/.:format`                                      | GET    | json    | users                    |
-- `/users/account-management.:format`                    | GET    | *       | users                    |
-- `/users/admins/.:format`                               | GET    | json    | users                    |
-- `/users/admins/user/:username.:format`                 | PUT    | *       | users                    |
-- `/users/admins/user/:username.:format`                 | DELETE | *       | users                    |
data UsersAPI mode = UsersAPI
    { userGetJson :: mode :- "user" :> Capture "username" (WithFormat UserName "json") :> Get '[JSON] ()
    , userPut :: mode :- "user" :> Capture "username" (WithAnyFormat UserName) :> Put '[TODO] ()
    , userDel :: mode :- "user" :> Capture "username" (WithAnyFormat UserName) :> Delete '[TODO] ()
    , userEnabledGet :: mode :- "user" :> Capture "username" UserName :> "enabled.json" :> Get '[JSON] ()
    , userEnabledPut :: mode :- "user" :> Capture "username" UserName :> "enabled.json" :> Put '[JSON] ()
    , userManageGet :: mode :- "user" :> Capture "username" UserName :> "manage" :> Get '[TODO] ()
    , userManagePost :: mode :- "user" :> Capture "username" UserName :> "manage" :> Post '[TODO] ()
    , usersGet :: mode :- "users" :> Get '[JSON] ()
    , usersAccountMgmt :: mode :- "users" :> "account-management" :> Get '[TODO] ()
    , usersAdminsGet :: mode :- "users" :> "admins" :> Get '[JSON] ()
    , usersAdminUserPut :: mode :- "users" :> "admins" :> "user" :> Capture "username" (WithAnyFormat UserName) :> Put '[TODO] ()
    , usersAdminUserDel :: mode :- "users" :> "admins" :> "user" :> Capture "username" (WithAnyFormat UserName) :> Delete '[TODO] ()
    }
    deriving stock (Generic)

-- `/package/:package/deprecated.:format`                 | GET    | json    | versions                 |
-- `/package/:package/deprecated.:format`                 | PUT    | json    | versions                 |
-- `/package/:package/preferred.:format`                  | GET    | json    | versions                 |
-- `/packages/deprecated.:format`                         | GET    | json    | versions                 |
-- `/packages/preferred-versions`                         | GET    | txt     | versions                 |
data VersionsAPI mode = VersionsAPI
    { packageDeprecatedGet :: mode :- "package" :> Capture "package" PackageName :> "deprecated.json" :> Get '[JSON] ()
    , packageDeprecatedPut :: mode :- "package" :> Capture "package" PackageName :> "deprecated.json" :> Put '[JSON] ()
    , packagePreferredGet :: mode :- "package" :> Capture "package" PackageName :> "preferred.json" :> Get '[JSON] ()
    , packagesDeprecatedGet :: mode :- "packages" :> "deprecated.json" :> Get '[JSON] ()
    , packagesPreferredVersions :: mode :- "packages" :> "preferred-versions" :> Get '[PlainText] ()
    }
    deriving stock (Generic)

-- `/package/:package/votes.:format`                      | GET    | json    | votes                    |
-- `/package/:package/votes.:format`                      | POST   | *       | votes                    |
-- `/package/:package/votes.:format`                      | DELETE | *       | votes                    |
-- `/packages/votes.:format`                              | GET    | json    | votes                    |
data VotesAPI mode = VotesAPI
    { packageVotesGet :: mode :- "package" :> Capture "package" PackageName :> "votes.json" :> Get '[JSON] ()
    , packageVotesPost :: mode :- "package" :> Capture "package" PackageName :> "votes" :> Post '[TODO] ()
    , packageVotesDel :: mode :- "package" :> Capture "package" PackageName :> "votes" :> Delete '[TODO] ()
    , packagesVotesGet :: mode :- "packages" :> "votes.json" :> Get '[JSON] ()
    }
    deriving stock (Generic)

data RedirectAPI mode = RedirectAPI
    { distrosRedir :: mode :- "distros" :> Capture "format" AnyFormat :> PermanentRedirect
    , redirDistroMaintainers :: mode :- Capture "package" PackageName :> "maintainers" :> "user" :> Capture "format" AnyFormat :> PermanentRedirect
    , redirPackageCandidates :: mode :- "package" :> Capture "package" PackageName :> "candidates" :> Capture "format" AnyFormat :> PermanentRedirect
    , redirPackagesCandidates :: mode :- "packages" :> "candidates" :> Capture "format" AnyFormat :> PermanentRedirect
    , redirPackageRevisions :: mode :- "package" :> Capture "package" PackageName :> "revisions" :> Capture "format" AnyFormat :> PermanentRedirect
    , redirPackages :: mode :- "packages" :> Capture "format" AnyFormat :> PermanentRedirect
    , redirCandidateDocs :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "docs" :> Capture "format" AnyFormat :> PermanentRedirect
    , redirMirrorers :: mode :- "packages" :> "mirrorers" :> Capture "format" AnyFormat :> PermanentRedirect
    , redirCandidateReports :: mode :- "package" :> Capture "package" PackageName :> "candidate" :> "reports" :> Capture "format" AnyFormat :> PermanentRedirect
    , redirReports :: mode :- "package" :> Capture "package" PackageName :> "reports" :> Capture "format" AnyFormat :> PermanentRedirect
    , redirPackageMaintainers :: mode :- "package" :> Capture "package" PackageName :> "maintainers" :> Capture "format" AnyFormat :> PermanentRedirect
    , redirTrustees :: mode :- "packages" :> "trustees" :> Capture "format" AnyFormat :> PermanentRedirect
    , redirUploaders :: mode :- "packages" :> "uploaders" :> Capture "format" AnyFormat :> PermanentRedirect
    , redirUsers :: mode :- "users" :> Capture "format" AnyFormat :> PermanentRedirect
    , redirUsersAdmins :: mode :- "users" :> "admins" :> Capture "format" AnyFormat :> PermanentRedirect
    , redirPackagesTags :: mode :- "packages" :> "tags" :> Capture "format" AnyFormat :> PermanentRedirect
    }
    deriving stock (Generic)

