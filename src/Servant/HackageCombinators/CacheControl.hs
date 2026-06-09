{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}

module Servant.HackageCombinators.CacheControl where

import Data.Hashable (Hashable(..))
import Control.Monad.Except (throwError)
import Data.Functor ((<&>))
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Servant.API
import Servant.Server hiding (respond)
import Text.Read (readMaybe)

-- | Automatically perform etag-based caching. This combinator /must/ be used
-- as the penultimate segment, just before a final 'Get', eg:
--
-- @... :> 'CacheControl' :> 'Get' cs a@
--
-- This constraint is required so that we can get our hands on the type @a@,
-- and use its 'Hashable' instance, rather than hash its projections (eg,
-- html), which are likely significantly more expensive.
data CacheControl

-- | Wrapper for describing the 'CacheControlSettings' of a 'CacheControl'
-- endpoint.
data WithCacheControl a = WithCacheControl [CacheControlSettings] a
  deriving stock Functor

data CacheControlSettings
  = MaxAge Int
  | SharedMaxAge Int
  | NoCache
  | NoStore
  | NoTransform
  | MustRevalidate
  | ProxyRevalidate
  | MustUnderstand
  | Private
  | Public
  | Immutable
  | StaleWhileRevalidate Int
  | StaleIfError Int

instance ToHttpApiData [CacheControlSettings] where
  toUrlPiece = T.intercalate ", " . fmap \case
    MaxAge i -> "max-age=" <> T.pack (show i)
    SharedMaxAge i -> "s-maxage=" <> T.pack (show i)
    NoCache -> "no-cache"
    NoStore -> "no-store"
    NoTransform -> "no-transform"
    MustRevalidate -> "must-revalidate"
    ProxyRevalidate -> "proxy-revalidate"
    MustUnderstand -> "must-understand"
    Private -> "private"
    Public -> "public"
    Immutable -> "immutable"
    StaleWhileRevalidate i -> "stale-while-revalidate=" <> T.pack (show i)
    StaleIfError i -> "stale-if-error=" <> T.pack (show i)


newtype ETag = ETag { getETag :: Int }
  deriving newtype (Eq, Show)

instance ToHttpApiData ETag where
  toUrlPiece = T.pack . show . show . getETag

instance FromHttpApiData ETag where
  parseUrlPiece t =
    maybe (Left "Not an ETag") Right $ do
      let s = T.unpack t
      s' <- readMaybe s
      i <- readMaybe s'
      pure $ ETag i

-- | This combinator is implemented by rewriting your API as if you had written
-- @rewrite@ instead of @CacheControl :> Get cs a@ (where @rewrite@ comes from
-- a constraint below.) This gives us a convenient means of getting our hands
-- on the relevant headers.
instance ( Hashable a
         , HasServer (Get cs a) ctx
         , rewrite ~
            ( Header "If-None-Match" ETag
              :> Get cs
                    (Headers '[ Header "Cache-Control" [CacheControlSettings]
                              , Header "ETag" ETag
                              ] a)
            )
         , HasServer rewrite ctx
         ) => HasServer (CacheControl :> Get cs a) ctx where
  type ServerT (CacheControl :> Get cs a) m = WithCacheControl (ServerT (Get cs a) m)
  hoistServerWithContext _ a b c = fmap (hoistServerWithContext (Proxy @(Get cs a)) a b) c
  route _ ctx app =
    route (Proxy @rewrite) ctx $
      app <&> \(WithCacheControl settings handler) ifnomatch -> do
        a <- handler
        let etag = ETag $ hash a
        case Just etag == ifnomatch of
          True ->
            -- Return @304 Not Modified@ when the etag matches
            throwError err304
          False ->
            pure $
              addHeader settings $
                addHeader etag a
