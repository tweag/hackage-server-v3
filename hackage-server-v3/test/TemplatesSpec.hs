{-# LANGUAGE TypeAbstractions     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

-- | Tests that that prove we can render the templates described in our API.
module TemplatesSpec where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Foldable
import Data.HashMap.Strict (fromList)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, typeRep)
import Hackage.API.Type
import Hackage.API.PackageDb ()
import Hackage.ServerM (filters)
import Servant.API
import Servant.EDE
import System.FilePath ((</>))
import Test.Hspec
import Test.QuickCheck (Arbitrary, forAll, property, arbitrary)
import Text.EDE (parseFile, renderWith, eitherResult)


spec :: Spec
spec = do
  let test = mkTemplatesSpec "templates"

  test $ Proxy @(ToServantApi PackageDbApi)


-- | Given a servant api, test that every template it mentions can be compiled
-- and rendered.
mkTemplatesSpec
    :: TemplateFiles TemplateTestable api
    => FilePath
    -- ^ Template directory
    -> Proxy api
    -> Spec
mkTemplatesSpec dir api =
  traverse_ (makeTemplateTest dir) $ reifyTemplates api


-- | Given a 'Template', test that the template it mentions can be compiled and
-- rendered.
makeTemplateTest
    :: FilePath
    -- ^ Template directory
    -> ReifiedTemplate TemplateTestable ()
    -> Spec
makeTemplateTest dir (ReifiedTemplate @_ @a pa ((dir </>) -> file) _) = do
  let mkObject = fromList . map (first Key.toText) . KeyMap.toList . toObject
  -- Attempt to compile the template once before starting the test.
  beforeAll (either error pure . eitherResult =<< parseFile file) $ do
    it (unwords [show $ typeRep pa, "->", file] ) $ \template ->
      -- If the templated compiled, we can try rendering it with synthetic data.
      -- The goal is to see if we can find any inputs which cause it to fail to
      -- render.
      property $ forAll arbitrary $ \(a :: a) ->
        eitherResult (renderWith filters template $ mkObject a) `shouldSatisfy` isRight


class (Arbitrary a, Typeable a, Show a) => TemplateTestable a
instance (Arbitrary a, Typeable a, Show a) => TemplateTestable a

