{-# LANGUAGE OverloadedStrings #-}

module Hackage.API.PackagesHTML where

import Data.Aeson (ToJSON)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Generics
import Hackage.Types
import Hackage.Utils
import Servant.API
import Servant.EDE
import Servant.Server.Generic (AsServerT)
import Test.QuickCheck
import Data.Text.Arbitrary ()

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
    -- { htmlPackagesGet :: mode :- "packages" :> Get '[HTML] ()
    -- , htmlPackagesPost :: mode :- "packages" :> Post '[HTML] ()
    -- , htmlPackagesBrowse :: mode :- "packages" :> "browse" :> Get '[HTML] ()
    -- , htmlPackagesDeprecated :: mode :- "packages" :> "deprecated.html" :> Get '[HTML] ()
    -- , htmlPackagesGraph :: mode :- "packages" :> "graph" :> Get '[HTML] ()
    -- , htmlPackagesGraphJson :: mode :- "packages" :> "graph.json" :> Get '[JSON] ()
    { htmlPackagesNames :: mode :- "packages" :> "names" :> Get '[HTML] PackageNames
    -- , htmlPackagesPreferred :: mode :- "packages" :> "preferred.html" :> Get '[HTML] ()
    -- , htmlPackagesRecentHtml :: mode :- "packages" :> "recent.html" :> Get '[HTML] ()
    -- , htmlPackagesRecentRss :: mode :- "packages" :> "recent.rss" :> Get '[RSS] ()
    -- , htmlPackagesRecentRevisionsHtml :: mode :- "packages" :> "recent" :> "revisions.html" :> Get '[HTML] ()
    -- , htmlPackagesRecentRevisionsRss :: mode :- "packages" :> "recent" :> "revisions.rss" :> Get '[RSS] ()
    -- , htmlPackagesReverse :: mode :- "packages" :> "reverse.html" :> Get '[HTML] ()
    -- , htmlPackagesSearch :: mode :- "packages" :> "search.html" :> Get '[HTML] ()
    -- , htmlPackagesTagGet :: mode :- "packages" :> "tag" :> CaptureExt "tag" Tag "html" :> Get '[HTML] ()
    -- , htmlPackagesTagAliasPut :: mode :- "packages" :> "tag" :> Capture "tag" Tag :> "alias" :> Put '[HTML] ()
    -- , htmlPackagesTagAliasEdit :: mode :- "packages" :> "tag" :> Capture "tag" Tag :> "alias" :> "edit" :> Get '[HTML] ()
    -- , htmlPackagesTagsGet :: mode :- "packages" :> "tags" :> Get '[HTML] ()
    -- , htmlPackagesTop :: mode :- "packages" :> "top.html" :> Get '[HTML] ()
    }
    deriving stock (Generic)


packagesHtmlServer :: PackagesHtmlAPI (AsServerT ServerM)
packagesHtmlServer = PackagesHtmlAPI
  {
    htmlPackagesNames = namesStub
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

