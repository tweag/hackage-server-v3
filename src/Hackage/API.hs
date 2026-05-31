{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Hackage.API where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits (Symbol)
import Servant.API
import Servant.HTML.Blaze
import Servant.Links

type HackageAPI = ()

type TODO = ()
type IndexTarball = ()

-- Content type aliases (to be filled in)
type TXT = ()
type CSV = ()
type RSS = ()
type XML = ()
type TAR = ()
type Tarball = ()
type Cabal = ()

-- Capture type aliases (to be filled in)
type DistroName = Text
type PackageName = Text
type Username = Text
type Nonce = Text
type Tag = Text
type ReportId = Text
type Revision = Text

data IndexTarballAPI mode = IndexTarballAPI
    { indexTarball :: mode :- "01-index.tar" :> Get '[TODO] IndexTarball
    , indexTarballGz :: mode :- "01-index.tar.gz" :> Get '[TODO] IndexTarball
    }

type UserId = Int

-- instance HasLink (f AsApi) where
--   type MkLink (f AsApi) a = f AsApi

{- | `/admin`              | GET    | html    | admin-frontend
| `/admin/account/:uid` | GET    | html    | admin-frontend
| `/admin/accounts`     | GET    | html    | admin-frontend
| `/admin/deauth`       | GET    | *       | core
| `/admin/log.:format`  | GET    | html    | admin-actions-log
| `/admin/resets`       | GET    | html    | admin-frontend
| `/admin/signups`      | GET    | html    | admin-frontend
-}
data AdminAPI mode = AdminApi
    { adminPage :: mode :- Get '[HTML] ()
    , adminUser :: mode :- "account" :> Capture "uid" UserId :> Get '[HTML] ()
    , adminAccounts :: mode :- "accounts" :> Get '[HTML] ()
    , adminDeauth :: mode :- "deauth" :> Get '[HTML, JSON] ()
    , adminLog :: mode :- "log.html" :> Get '[HTML] ()
    , adminResets :: mode :- "resets" :> Get '[HTML] ()
    , adminSignups :: mode :- "signups" :> Get '[HTML] ()
    }
    deriving stock (Generic)

{- | `/distro/:distro`                                      | PUT    | *       | distro
| `/distro/:distro`                                      | DELETE | *       | distro
| `/distro/:distro/package/:package.:format`             | GET    | txt     | distro
| `/distro/:distro/package/:package.:format`             | PUT    | *       | distro
| `/distro/:distro/package/:package.:format`             | DELETE | *       | distro
| `/distro/:distro/packages.:format`                     | GET    | csv     | distro
| `/distro/:distro/packages.:format`                     | GET    | txt     | distro
| `/distro/:distro/packages.:format`                     | PUT    | csv     | distro
| `/distro/:package/maintainers/.:format`                | GET    | json    | distro
| `/distro/:package/maintainers/user/:username.:format`  | PUT    | *       | distro
| `/distro/:package/maintainers/user/:username.:format`  | DELETE | *       | distro
-}
data DistroAPI mode = DistroAPI
    { distroPut :: mode :- IsAdmin :> Capture "distro" DistroName :> Put '[TODO] ()
    , distroDel :: mode :- IsAdmin :> Capture "distro" DistroName :> Delete '[TODO] ()
    , distroPkgGet :: mode :- Capture "distro" DistroName :> "package" :> Capture "package" (WithFormat PackageName "txt") :> Get '[TXT] ()
    , distroPkgPut :: mode :- Capture "distro" DistroName :> "package" :> Capture "package" (WithFormat PackageName "txt") :> Put '[TXT] ()
    , distroPkgDel :: mode :- Capture "distro" DistroName :> "package" :> Capture "package" (WithFormat PackageName "txt") :> Delete '[TXT] ()
    , distroPkgsGetCsv :: mode :- Capture "distro" DistroName :> "packages.csv" :> Get '[CSV] ()
    , distroPkgsPutCsv :: mode :- Capture "distro" DistroName :> "packages.csv" :> Put '[CSV] ()
    , distroPkgsGetTxt :: mode :- Capture "distro" DistroName :> "packages.txt" :> Get '[TXT] ()
    , distroPkgMaintainersPut :: mode :- Capture "package" PackageName :> "maintainers" :> "user" :> Capture "username" (WithAnyFormat Username) :> Put '[] ()
    , distroPkgMaintainersDel :: mode :- Capture "package" PackageName :> "maintainers" :> "user" :> Capture "username" (WithAnyFormat Username) :> Delete '[] ()
    }
    deriving stock (Generic)

{- | `/distros/.:format` | GET    | txt     | distro
| `/distros/.:format` | POST   | *       | distro
-}
data DistrosAPI mode = DistrosAPI
    { distrosGet :: mode :- IsAdmin :> Get '[TXT] ()
    , distrosPost :: mode :- IsAdmin :> Post '[] ()
    , distrosRedir :: mode :- Redirect
    }
    deriving stock (Generic)

data AllAPI mode = AllAPI
    { adminApi :: mode :- "admin" :> NamedRoutes AdminAPI
    }
    deriving stock (Generic)

type API = NamedRoutes AllAPI

api :: Proxy API
api = Proxy

type ApiAPI =
    "api.html" :> Get '[HTML] ()
        :<|> "api.json" :> Get '[JSON] ()

type WithFormat :: Type -> Symbol -> Type
newtype WithFormat a b = WithFormat {unWithFormat :: a}

type WithAnyFormat :: Type -> Type
newtype WithAnyFormat a = WithAnyFormat {unWithAnyFormat :: a}

data Redirect

data IsAdmin

{- | `/accounts`                                            | GET    | *       | static-files             |
| `/hackageErrorPage`                                    | GET    | *       | static-files             |
| `/index`                                               | GET    | *       | static-files             |
| `/mirrors.json`                                        | GET    | json    | security                 |
| `/package/`                                            | GET    | *       | core                     |
| `/package/:package.:format`                            | GET    | html    | html                     |
| `/package/:package.:format`                            | GET    | json    | package-info-json        |
| `/package/:package.rss`                                | GET    | rss     | package feed             |
| `/package/:package/:cabal.cabal`                       | GET    | cabal   | core                     |
| `/package/:package/:cabal.cabal`                       | PUT    | *       | mirror                   |
| `/package/:package/:cabal.cabal/edit`                  | GET    | html    | edit-cabal-files         |
| `/package/:package/:cabal.cabal/edit`                  | POST   | html    | edit-cabal-files         |
| `/package/:package/:tarball.tar.gz`                    | GET    | tarball | core                     |
| `/package/:package/:tarball.tar.gz`                    | PUT    | *       | mirror                   |
| `/package/:package/analytics-pixels.:format`           | GET    | html    | html                     |
| `/package/:package/analytics-pixels.:format`           | POST   | html    | html                     |
| `/package/:package/analytics-pixels.:format`           | DELETE | html    | html                     |
| `/package/:package/candidate.:format`                  | GET    | html    | html                     |
| `/package/:package/candidate.:format`                  | PUT    | html    | html                     |
| `/package/:package/candidate.:format`                  | DELETE | html    | html                     |
| `/package/:package/candidate/:cabal.cabal`             | GET    | cabal   | candidates               |
| `/package/:package/candidate/:tarball.tar.gz`          | GET    | tarball | candidates               |
| `/package/:package/candidate/changelog.:format`        | GET    | html    | candidates               |
| `/package/:package/candidate/changelog.:format`        | GET    | txt     | candidates               |
| `/package/:package/candidate/delete.:format`           | GET    | html    | html                     |
| `/package/:package/candidate/delete.:format`           | POST   | html    | html                     |
| `/package/:package/candidate/dependencies`             | GET    | html    | html                     |
| `/package/:package/candidate/docs.:format`             | GET    | tar     | documentation-candidates |
| `/package/:package/candidate/docs.:format`             | PUT    | html    | html                     |
| `/package/:package/candidate/docs.:format`             | PUT    | tar     | documentation-candidates |
| `/package/:package/candidate/docs.:format`             | DELETE | *       | documentation-candidates |
| `/package/:package/candidate/docs.:format`             | DELETE | html    | html                     |
| `/package/:package/candidate/docs/...`                 | GET    | *       | documentation-candidates |
| `/package/:package/candidate/maintain`                 | GET    | html    | html                     |
| `/package/:package/candidate/maintain/docs`            | GET    | html    | html                     |
| `/package/:package/candidate/publish.:format`          | GET    | html    | html                     |
| `/package/:package/candidate/publish.:format`          | POST   | html    | html                     |
| `/package/:package/candidate/reports/.:format`         | GET    | txt     | reports-candidates       |
| `/package/:package/candidate/reports/.:format`         | POST   | *       | reports-candidates       |
| `/package/:package/candidate/reports/.:format`         | PUT    | json    | reports-candidates       |
| `/package/:package/candidate/reports/:id.:format`      | GET    | txt     | reports-candidates       |
| `/package/:package/candidate/reports/:id.:format`      | DELETE | *       | reports-candidates       |
| `/package/:package/candidate/reports/:id/log`          | GET    | txt     | reports-candidates       |
| `/package/:package/candidate/reports/:id/log`          | PUT    | *       | reports-candidates       |
| `/package/:package/candidate/reports/:id/log`          | DELETE | *       | reports-candidates       |
| `/package/:package/candidate/reports/:id/test`         | GET    | txt     | reports-candidates       |
| `/package/:package/candidate/reports/:id/test`         | PUT    | *       | reports-candidates       |
| `/package/:package/candidate/reports/:id/test`         | DELETE | *       | reports-candidates       |
| `/package/:package/candidate/reports/reset/`           | GET    | *       | reports-candidates       |
| `/package/:package/candidate/reports/testsEnabled/`    | GET    | json    | reports-candidates       |
| `/package/:package/candidate/reports/testsEnabled/`    | POST   | *       | reports-candidates       |
| `/package/:package/candidate/src/...`                  | GET    | *       | candidates               |
| `/package/:package/candidate/upload`                   | GET    | html    | html                     |
| `/package/:package/candidates/.:format`                | GET    | html    | html                     |
| `/package/:package/candidates/.:format`                | GET    | json    | candidates               |
| `/package/:package/candidates/.:format`                | POST   | *       | html                     |
| `/package/:package/candidates/delete.:format`          | GET    | html    | html                     |
| `/package/:package/candidates/delete.:format`          | POST   | html    | html                     |
| `/package/:package/changelog.:format`                  | GET    | html    | package-contents         |
| `/package/:package/changelog.:format`                  | GET    | txt     | package-contents         |
| `/package/:package/dependencies`                       | GET    | html    | html                     |
| `/package/:package/deprecated.:format`                 | GET    | html    | html                     |
| `/package/:package/deprecated.:format`                 | GET    | json    | versions                 |
| `/package/:package/deprecated.:format`                 | PUT    | html    | html                     |
| `/package/:package/deprecated.:format`                 | PUT    | json    | versions                 |
| `/package/:package/deprecated/edit`                    | GET    | html    | html                     |
| `/package/:package/distro-monitor.:format`             | GET    | html    | html                     |
| `/package/:package/docs.:format`                       | GET    | tar     | documentation-core       |
| `/package/:package/docs.:format`                       | PUT    | html    | html                     |
| `/package/:package/docs.:format`                       | PUT    | tar     | documentation-core       |
| `/package/:package/docs.:format`                       | DELETE | *       | documentation-core       |
| `/package/:package/docs.:format`                       | DELETE | html    | html                     |
| `/package/:package/docs/...`                           | GET    | *       | documentation-core       |
| `/package/:package/maintain`                           | GET    | html    | html                     |
| `/package/:package/maintain/docs`                      | GET    | html    | html                     |
| `/package/:package/maintainers/.:format`               | GET    | html    | html                     |
| `/package/:package/maintainers/.:format`               | GET    | json    | upload                   |
| `/package/:package/maintainers/.:format`               | POST   | html    | html                     |
| `/package/:package/maintainers/edit`                   | GET    | html    | html                     |
| `/package/:package/maintainers/user/:username.:format` | PUT    | *       | upload                   |
| `/package/:package/maintainers/user/:username.:format` | DELETE | *       | upload                   |
| `/package/:package/maintainers/user/:username.:format` | DELETE | html    | html                     |
| `/package/:package/preferred.:format`                  | GET    | html    | html                     |
| `/package/:package/preferred.:format`                  | GET    | json    | versions                 |
| `/package/:package/preferred.:format`                  | PUT    | html    | html                     |
| `/package/:package/preferred/edit`                     | GET    | html    | html                     |
| `/package/:package/readme.:format`                     | GET    | html    | package-contents         |
| `/package/:package/readme.:format`                     | GET    | txt     | package-contents         |
| `/package/:package/reports/.:format`                   | GET    | html    | html                     |
| `/package/:package/reports/.:format`                   | GET    | txt     | reports-core             |
| `/package/:package/reports/.:format`                   | POST   | *       | reports-core             |
| `/package/:package/reports/.:format`                   | PUT    | json    | reports-core             |
| `/package/:package/reports/:id.:format`                | GET    | html    | html                     |
| `/package/:package/reports/:id.:format`                | GET    | txt     | reports-core             |
| `/package/:package/reports/:id.:format`                | DELETE | *       | reports-core             |
| `/package/:package/reports/:id/log`                    | GET    | txt     | reports-core             |
| `/package/:package/reports/:id/log`                    | PUT    | *       | reports-core             |
| `/package/:package/reports/:id/log`                    | DELETE | *       | reports-core             |
| `/package/:package/reports/:id/test`                   | GET    | txt     | reports-core             |
| `/package/:package/reports/:id/test`                   | PUT    | *       | reports-core             |
| `/package/:package/reports/:id/test`                   | DELETE | *       | reports-core             |
| `/package/:package/reports/reset/`                     | GET    | *       | reports-core             |
| `/package/:package/reports/testsEnabled/`              | GET    | html    | html                     |
| `/package/:package/reports/testsEnabled/`              | GET    | json    | reports-core             |
| `/package/:package/reports/testsEnabled/`              | POST   | *       | reports-core             |
| `/package/:package/reverse.:format`                    | GET    | html    | html                     |
| `/package/:package/reverse/flat.:format`               | GET    | html    | html                     |
| `/package/:package/reverse/old.:format`                | GET    | html    | html                     |
| `/package/:package/reverse/verbose.:format`            | GET    | html    | html                     |
| `/package/:package/revision/:revision.:format`         | GET    | cabal   | core                     |
| `/package/:package/revision/:revision.:format`         | GET    | json    | package-info-json        |
| `/package/:package/revisions/.:format`                 | GET    | html    | html                     |
| `/package/:package/revisions/.:format`                 | GET    | json    | core                     |
| `/package/:package/src/...`                            | GET    | *       | package-contents         |
| `/package/:package/tags.:format`                       | GET    | html    | html                     |
| `/package/:package/tags.:format`                       | PUT    | html    | html                     |
| `/package/:package/tags/edit`                          | GET    | html    | html                     |
| `/package/:package/upload-time`                        | GET    | *       | mirror                   |
| `/package/:package/upload-time`                        | PUT    | *       | mirror                   |
| `/package/:package/uploader`                           | GET    | *       | mirror                   |
| `/package/:package/uploader`                           | PUT    | *       | mirror                   |
| `/package/:package/votes.:format`                      | GET    | json    | votes                    |
| `/package/:package/votes.:format`                      | POST   | *       | votes                    |
| `/package/:package/votes.:format`                      | DELETE | *       | votes                    |
| `/packages/.:format`                                   | GET    | html    | html                     |
| `/packages/.:format`                                   | GET    | json    | core                     |
| `/packages/.:format`                                   | POST   | html    | html                     |
| `/packages/.:format`                                   | POST   | txt     | upload                   |
| `/packages/browse`                                     | GET    | html    | html                     |
| `/packages/candidates/.:format`                        | GET    | html    | html                     |
| `/packages/candidates/.:format`                        | GET    | json    | candidates               |
| `/packages/candidates/.:format`                        | POST   | html    | html                     |
| `/packages/candidates/.:format`                        | POST   | txt     | candidates               |
| `/packages/candidates/docs.:format`                    | GET    | json    | documentation-candidates |
| `/packages/candidates/upload`                          | GET    | html    | html                     |
| `/packages/deauth`                                     | GET    | *       | core                     |
| `/packages/deprecated.:format`                         | GET    | html    | html                     |
| `/packages/deprecated.:format`                         | GET    | json    | versions                 |
| `/packages/docs.:format`                               | GET    | json    | documentation-core       |
| `/packages/downloads.:format`                          | GET    | csv     | download                 |
| `/packages/downloads.:format`                          | PUT    | csv     | download                 |
| `/packages/graph`                                      | GET    | html    | html                     |
| `/packages/graph.json`                                 | GET    | json    | html                     |
| `/packages/hoogle.tar.gz`                              | GET    | tarball | hoogle-data              |
| `/packages/index.tar.gz`                               | GET    | tarball | core                     |
| `/packages/mirrorers/.:format`                         | GET    | html    | html                     |
| `/packages/mirrorers/.:format`                         | GET    | json    | mirror                   |
| `/packages/mirrorers/.:format`                         | POST   | html    | html                     |
| `/packages/mirrorers/edit`                             | GET    | html    | html                     |
| `/packages/mirrorers/user/:username.:format`           | PUT    | *       | mirror                   |
| `/packages/mirrorers/user/:username.:format`           | DELETE | *       | mirror                   |
| `/packages/mirrorers/user/:username.:format`           | DELETE | html    | html                     |
| `/packages/names`                                      | GET    | html    | html                     |
| `/packages/noscript-search`                            | GET    | html    | search/browse backend    |
| `/packages/noscript-search`                            | POST   | html    | search/browse backend    |
| `/packages/opensearch.xml`                             | GET    | xml     | search                   |
| `/packages/preferred-versions`                         | GET    | txt     | versions                 |
| `/packages/preferred.:format`                          | GET    | html    | html                     |
| `/packages/recent.:format`                             | GET    | html    | html                     |
| `/packages/recent.:format`                             | GET    | rss     | html                     |
| `/packages/recent/revisions.:format`                   | GET    | html    | html                     |
| `/packages/recent/revisions.:format`                   | GET    | rss     | html                     |
| `/packages/reverse.:format`                            | GET    | html    | html                     |
| `/packages/search`                                     | POST   | json    | search/browse backend    |
| `/packages/search.:format`                             | GET    | html    | html                     |
| `/packages/search.:format`                             | GET    | json    | search                   |
| `/packages/tag/:tag.:format`                           | GET    | html    | html                     |
| `/packages/tag/:tag/alias`                             | PUT    | html    | html                     |
| `/packages/tag/:tag/alias/edit`                        | GET    | html    | html                     |
| `/packages/tags/.:format`                              | GET    | html    | html                     |
| `/packages/top.:format`                                | GET    | html    | html                     |
| `/packages/top.:format`                                | GET    | json    | download                 |
| `/packages/trustees/.:format`                          | GET    | html    | html                     |
| `/packages/trustees/.:format`                          | GET    | json    | upload                   |
| `/packages/trustees/.:format`                          | POST   | html    | html                     |
| `/packages/trustees/edit`                              | GET    | html    | html                     |
| `/packages/trustees/user/:username.:format`            | PUT    | *       | upload                   |
| `/packages/trustees/user/:username.:format`            | DELETE | *       | upload                   |
| `/packages/trustees/user/:username.:format`            | DELETE | html    | html                     |
| `/packages/upload`                                     | GET    | html    | html                     |
| `/packages/uploaders/.:format`                         | GET    | html    | html                     |
| `/packages/uploaders/.:format`                         | GET    | json    | upload                   |
| `/packages/uploaders/.:format`                         | POST   | html    | html                     |
| `/packages/uploaders/edit`                             | GET    | html    | html                     |
| `/packages/uploaders/user/:username.:format`           | PUT    | *       | upload                   |
| `/packages/uploaders/user/:username.:format`           | DELETE | *       | upload                   |
| `/packages/uploaders/user/:username.:format`           | DELETE | html    | html                     |
| `/packages/votes.:format`                              | GET    | json    | votes                    |
| `/root.json`                                           | GET    | json    | security                 |
| `/server-status/memory.:format`                        | GET    | html    | serverapi                |
| `/server-status/tarindices.:format`                    | GET    | json    | tarIndexCache            |
| `/server-status/tarindices.:format`                    | DELETE | *       | tarIndexCache            |
| `/sitemap/:filename`                                   | GET    | xml     | sitemap                  |
| `/sitemap_index.xml`                                   | GET    | xml     | sitemap                  |
| `/snapshot.json`                                       | GET    | json    | security                 |
| `/static/...`                                          | GET    | *       | static-files             |
| `/timestamp.json`                                      | GET    | json    | security                 |
| `/upload`                                              | GET    | *       | static-files             |
| `/user/:user/deauth`                                   | GET    | *       | core                     |
| `/user/:username.:format`                              | GET    | html    | html                     |
| `/user/:username.:format`                              | GET    | json    | users                    |
| `/user/:username.:format`                              | PUT    | *       | users                    |
| `/user/:username.:format`                              | DELETE | *       | users                    |
| `/user/:username/admin-info.:format`                   | GET    | json    | user-details             |
| `/user/:username/admin-info.:format`                   | PUT    | json    | user-details             |
| `/user/:username/admin-info.:format`                   | DELETE | *       | user-details             |
| `/user/:username/analytics-pixels.:format`             | GET    | html    | html                     |
| `/user/:username/analytics-pixels.:format`             | POST   | html    | html                     |
| `/user/:username/analytics-pixels.:format`             | DELETE | html    | html                     |
| `/user/:username/enabled.:format`                      | GET    | json    | users                    |
| `/user/:username/enabled.:format`                      | PUT    | json    | users                    |
| `/user/:username/endorse`                              | GET    | html    | endorse                  |
| `/user/:username/endorse`                              | POST   | html    | endorse                  |
| `/user/:username/manage.:format`                       | GET    | *       | users                    |
| `/user/:username/manage.:format`                       | POST   | *       | users                    |
| `/user/:username/name-contact.:format`                 | GET    | html    | user-details             |
| `/user/:username/name-contact.:format`                 | GET    | json    | user-details             |
| `/user/:username/name-contact.:format`                 | PUT    | json    | user-details             |
| `/user/:username/name-contact.:format`                 | DELETE | *       | user-details             |
| `/user/:username/notify.:format`                       | GET    | html    | user-notify              |
| `/user/:username/notify.:format`                       | GET    | json    | user-notify              |
| `/user/:username/notify.:format`                       | PUT    | json    | user-notify              |
| `/user/:username/password.:format`                     | GET    | html    | html                     |
| `/user/:username/password.:format`                     | PUT    | html    | html                     |
| `/users/.:format`                                      | GET    | html    | html                     |
| `/users/.:format`                                      | GET    | json    | users                    |
| `/users/.:format`                                      | POST   | html    | html                     |
| `/users/account-management.:format`                    | GET    | *       | users                    |
| `/users/admins/.:format`                               | GET    | html    | html                     |
| `/users/admins/.:format`                               | GET    | json    | users                    |
| `/users/admins/.:format`                               | POST   | html    | html                     |
| `/users/admins/edit`                                   | GET    | html    | html                     |
| `/users/admins/user/:username.:format`                 | PUT    | *       | users                    |
| `/users/admins/user/:username.:format`                 | DELETE | *       | users                    |
| `/users/admins/user/:username.:format`                 | DELETE | html    | html                     |
| `/users/password-reset`                                | GET    | *       | user-signup-reset        |
| `/users/password-reset`                                | POST   | *       | user-signup-reset        |
| `/users/password-reset/:nonce`                         | GET    | *       | user-signup-reset        |
| `/users/password-reset/:nonce`                         | POST   | *       | user-signup-reset        |
| `/users/register`                                      | GET    | html    | html                     |
| `/users/register-request`                              | GET    | *       | user-signup-reset        |
| `/users/register-request`                              | POST   | *       | user-signup-reset        |
| `/users/register-request/:nonce`                       | GET    | *       | user-signup-reset        |
| `/users/register-request/:nonce`                       | POST   | *       | user-signup-reset        |
| `/users/register/captcha`                              | GET    | json    | user-signup-reset        |
-}
