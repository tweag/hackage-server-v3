{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.HackageCombinators.DynamicGet
  ( DynamicGet
  , OneOf(..)
  ) where

import Data.Kind (Type)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Proxy (Proxy (..))
import Network.HTTP.Media (renderHeader)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (ok200)
import Network.Wai (responseLBS)
import Servant.API
import Servant.EDE
import Servant.Server hiding (respond)
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.Router
import Servant.Server.Internal.RouteResult


-- | An heterogeneous variant of pairs of content types and values. For
-- example, a value of @'OneOf' [ '(JSON, Int), '(PlainText, Text) ]@ is either
-- an integer served as JSON or text served as plaintext. We can construct the
-- two values as:
--
-- - @'HHere' (Proxy :: Proxy JSON) 42@
-- - @'HThere' ('HHere' (Proxy :: Proxy PlainText) "hello world")@
type OneOf :: [(k, Type)] -> Type
data OneOf ts where
  -- | 'HHere' is the base case of 'OneOf'. It creates a pair in the hlist with
  -- a polymorphic tail.
  HHere
      :: (LoadedTemplates => MimeRender ct a)
      => Proxy ct
      -> a
      -> OneOf ( '(ct, a) ': as )
  -- | 'HThere' pushes an existing 'OneOf' down the stack, meaning it prepends
  -- a content type pair to the beginning of the list. That is to say, you
  -- should invoke 'HThere' once for every choice you didn't take in the
  -- variant.
  HThere
      :: OneOf as
      -> OneOf ( '(ct, a) ': as )


instance Eq (OneOf '[]) where
  a == _ = case a of

instance (Eq a, Eq (OneOf ts)) => Eq (OneOf ('(ct, a) ': ts)) where
  HHere _ a == HHere _ b = a == b
  HThere as == HThere bs = as == bs
  _ == _ = False

instance Show (OneOf '[]) where
  show a = case a of

instance (Show a, Show (OneOf ts)) => Show (OneOf ('(ct, a) ': ts)) where
  show (HHere _ a) = show a
  show (HThere as) = show as


-- | Invoke a continuation with the content type and value for the single value
-- in a 'OneOf'.
mimeRenderOneOf
    :: forall k (ts :: [(k, Type)]) r
     . OneOf ts
    -> (forall (ct :: k) a. (LoadedTemplates => MimeRender ct a) => Proxy ct -> a -> r)
    -> r
mimeRenderOneOf (HHere ct a) k = k ct a
mimeRenderOneOf (HThere as) k = mimeRenderOneOf as k

-- | A @GET@ endpoint whose handler dynamically chooses at runtime between the
-- given alternatives for its content type and value. response value at
-- runtime, rather than having it fixed in the API type.
type DynamicGet :: [(k, Type)] -> Type
data DynamicGet ts

instance LoadedTemplates => HasServer (DynamicGet ts) ctx where
  type ServerT (DynamicGet ts) m = m (OneOf ts)
  hoistServerWithContext _ _ nt = nt
  route _ _ handler = RawRouter $ \env req resp -> runResourceT $ do
    routeResult <- runDelayed handler env req
    liftIO $ case routeResult of
      Fail e      -> resp $ Fail e
      FailFatal e -> resp $ FailFatal e
      Route h     -> do
        result <- runHandler h
        resp $ case result of
          Left err -> Fail err
          Right oo -> mimeRenderOneOf oo $ \ct a ->
            Route $
              responseLBS
                ok200
                [(hContentType, renderHeader $ contentType ct)] $
                mimeRender ct a


instance TemplateFiles c (DynamicGet '[]) where
  reifyTemplates = mempty

instance ( ContentTemplateFiles c '[ct] a
         , TemplateFiles c (DynamicGet ts)
         ) => TemplateFiles c (DynamicGet ('(ct, a) ': ts))
    where
  reifyTemplates _ = mconcat
    [ contentTemplatesFor (Proxy @'[ct]) (Proxy @a)
    , reifyTemplates $ Proxy @(DynamicGet ts)
    ]

instance HasLink (DynamicGet a) where
  type MkLink (DynamicGet a) r = r
  toLink toA _ = toA

