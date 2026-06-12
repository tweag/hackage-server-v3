{-# LANGUAGE TypeAbstractions     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

-- | Tests that that prove we can render the templates described in our API.
module TemplatesSpec where

import Data.Typeable (Typeable, typeRep)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Foldable
import Data.HashMap.Strict (fromList)
import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))
import Hackage.API.PackagesHTML
import Servant.API
import Servant.EDE
import System.FilePath ((</>))
import Test.Hspec
import Test.QuickCheck (Arbitrary, forAll, property, arbitrary)
import Text.EDE (parseFile, render, eitherResult)


spec :: Spec
spec = do
  let test = mkTemplatesSpec "templates"

  test $ Proxy @(ToServantApi PackagesHtmlAPI)


-- | Given a servant api, test that every template it mentions can be compiled
-- and rendered.
mkTemplatesSpec
    :: GetTemplates api
    => FilePath
    -- ^ Template directory
    -> Proxy api
    -> Spec
mkTemplatesSpec dir api =
  traverse_ (makeTemplateTest dir) $ getTemplates api


-- | Given a 'Template', test that the template it mentions can be compiled and
-- rendered.
makeTemplateTest
    :: FilePath
    -- ^ Template directory
    -> Template
    -> Spec
makeTemplateTest dir (Template @a pa ((dir </>) -> file)) = do
  let mkObject = fromList . map (first Key.toText) . KeyMap.toList . toObject
  -- Attempt to compile the template once before starting the test.
  beforeAll (either error pure . eitherResult =<< parseFile file) $ do
    it (unwords [show $ typeRep pa, "->", file] ) $ \template ->
      -- If the templated compiled, we can try rendering it with synthetic data.
      -- The goal is to see if we can find any inputs which cause it to fail to
      -- render.
      property $ forAll arbitrary $ \(a :: a) ->
        eitherResult (render template $ mkObject a) `shouldSatisfy` isRight


-- | Helper data structure for implementing 'GetTemplates'. This exists
-- to package up the existential type @a@ and its dictionaries.
data Template where
  Template
    :: (Arbitrary a, Typeable a, Show a, ToObject a)
    => Proxy a
    -> FilePath
    -- ^ The filepath of the template
    -> Template


-- | Traverse a servant API, building a 'Template' for each of its mentioned
-- templates.
type GetTemplates :: k -> Constraint
class GetTemplates api where
  getTemplates :: Proxy api -> [Template]

instance (GetTemplates a, GetTemplates b) => GetTemplates (a :<|> b) where
  getTemplates _ = getTemplates (Proxy @a) <> getTemplates (Proxy @b)

instance (GetTemplates api) => GetTemplates (a :> api) where
  getTemplates _ = getTemplates $ Proxy @api

instance GetContentTemplates c a => GetTemplates (Verb m s c a) where
  getTemplates _ = getContentTemplates (Proxy @c) (Proxy @a)

instance GetTemplates Raw where
  getTemplates _ = mempty

instance GetTemplates (ToServantApi a) => GetTemplates (NamedRoutes a) where
  getTemplates _ = getTemplates (Proxy @(ToServantApi a))


-- | Like 'GetTemplates', but dispatches on content types rather than on servant
-- apis.
type GetContentTemplates :: [Type] -> Type -> Constraint
class GetContentTemplates c a where
  getContentTemplates :: Proxy c -> Proxy a -> [Template]

instance GetContentTemplates '[] a where
  getContentTemplates _ _ = mempty

instance {-# OVERLAPPING #-}
    (Arbitrary a, Show a, ToObject a, Typeable a, HasTemplate HTML a, GetContentTemplates cs a)
    => GetContentTemplates (HTML ': cs) a where
  getContentTemplates _ pa = Template pa (templateFor (Proxy @HTML) pa) : getContentTemplates (Proxy @cs) pa

instance {-# OVERLAPPING #-}
    (Arbitrary a, Show a, ToObject a, Typeable a, HasTemplate c a, GetContentTemplates cs a)
    => GetContentTemplates (Tpl c ': cs) a where
  getContentTemplates _ pa = Template pa (templateFor (Proxy @c) pa) : getContentTemplates (Proxy @cs) pa

instance {-# OVERLAPPABLE #-} (GetContentTemplates cs a) => GetContentTemplates (c ': cs) a where
  getContentTemplates _ pa = getContentTemplates (Proxy @cs) pa

