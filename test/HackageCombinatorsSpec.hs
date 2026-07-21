{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module HackageCombinatorsSpec where

import Servant.EDE (LoadedTemplates, unsafeLoadTemplates)
import Crypto.Hash qualified as Crypto
import Data.ByteString (ByteString)
import Data.Proxy (Proxy(..))
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics
import Servant.API
import Servant.HackageAuth (hackageRealm)
import Servant.HackageCombinators
import Servant.Links (fieldLink)
import Servant.Server
import Test.Hspec
import Test.Hspec.Wai


data API mode = API
  { redirect          :: mode :- "redirect" :> "in" :> Capture "capture" String :> PermanentRedirect
  , redirectTarget    :: mode :- "redirect" :> "out" :> Capture "capture" String :> Get '[JSON] String
  , formatJson        :: mode :- "format" :> CaptureExt "something" String "json" :> Get '[JSON] String
  , formatTarGz       :: mode :- "format" :> CaptureExt "something" String "tar.gz" :> Get '[JSON] String
  , auth              :: mode :- "auth" :> HackageAuth :> Get '[JSON] ()
  , cacheControl      :: mode :- "cache" :> CacheControl :> Get '[JSON] ()
  , userDomain        :: mode :- "user" :> UserDomain :> Get '[JSON] ()
  , negotiableContent :: mode :- NegotiableContent :> "negotiable" :> Capture "something" String :> Get '[PlainText, JSON] String
  , whitelistDigest   :: mode :- "whitelist" :> Capture "something" String :> WhitelistDigest '[JSON] String
  , dynamicGet        :: mode :- "dynamic" :> Capture "switch" Bool :> DynamicGet '[ '(JSON, Int), '(PlainText, T.Text)]
  }
  deriving stock Generic


unsafeIgnoreTemplates :: (LoadedTemplates => r) -> IO r
unsafeIgnoreTemplates k = do
  Right x <- unsafeLoadTemplates (Proxy @EmptyAPI) [] "." $ pure k
  pure x


spec :: Spec
spec =
  with (
    unsafeIgnoreTemplates $
      serveWithContext
        (Proxy @(NamedRoutes API))
        (UserDomain "my.user.domain"
          :. hackageAuthHandler hackageRealm undefined
          :. EmptyContext
        )
        (API
          { redirect = fieldLink redirectTarget
          , redirectTarget = pure
          , formatJson = pure
          , formatTarGz = pure
          , auth = const $ pure ()
          , cacheControl = WithCacheControl [MaxAge 1234, SharedMaxAge 4321] $ pure ()
          , userDomain = pure ()
          , negotiableContent = pure
          , whitelistDigest = \str ->
              WithWhitelistDigest
                (S.singleton $ Crypto.hashWith @ByteString Crypto.SHA256 $ "\"acceptable\"")
                (pure str)
          , dynamicGet = \case
              False -> pure $ HHere Proxy 15
              True  -> pure $ HThere $ HHere Proxy "hello"
          }
        )) $ do

  describe "redirect" $ do
    it "should redirect" $ do
      get "/redirect/in/field" `shouldRespondWith` 308
        { matchHeaders =
            [ "Location" <:> "/redirect/out/field"
            ]
        }

  describe "format" $ do
    it "should match when capturing the format" $ do
      get "/format/hello.json" `shouldRespondWith` "\"hello\""
    it "should match when capturing the format with two dots" $ do
      get "/format/hello.tar.gz" `shouldRespondWith` "\"hello\""
    it "should not match when on other formats" $ do
      get "/format/hello.txt" `shouldRespondWith` 404

  describe "auth" $ do
    it "should do auth" $ do
      get "/auth" `shouldRespondWith` 401

  describe "negotiable content" $ do
    it "should dispatch without an extension" $ do
      get "/negotiable/hello" `shouldRespondWith` "hello"
    it "should dispatch txt based on extension" $ do
      get "/negotiable/hello.txt" `shouldRespondWith` "hello"
    it "should dispatch json based on extension" $ do
      get "/negotiable/hello.json" `shouldRespondWith` "\"hello\""
    it "should not dispatch html based on extension" $ do
      get "/negotiable/hello.html" `shouldRespondWith` 406
    it "should still dispatch with an empty segment" $ do
      get "/negotiable/hello/.json" `shouldRespondWith` "\"hello\""

  describe "cache control" $ do
    it "should return etags and cache control settings" $ do
      get "/cache" `shouldRespondWith` 200
        { matchHeaders =
            [ "ETag" <:> "\"0\""
            , "Cache-Control" <:> "max-age=1234, s-maxage=4321"
            ]
        }
    it "should return 304 when Etag matches" $ do
      request "GET" "/cache" [("If-None-Match", "\"0\"")] "" `shouldRespondWith` 304
    it "should return 200 when Etag doesn't match" $ do
      request "GET" "/cache" [("If-None-Match", "\"1\"")] "" `shouldRespondWith` 200

  describe "user domain" $ do
    it "should 301 when running on the wrong domain" $ do
      request "GET" "/user" [("Host", "some.domain")] "" `shouldRespondWith` 301
        { matchHeaders =
            [ "Location" <:> "my.user.domain/user"
            ]
        }
    it "should 200 when running on the user domain" $ do
      request "GET" "/user" [("Host", "my.user.domain")] "" `shouldRespondWith` 200

  describe "whitelist digest" $ do
    it "should return 200 for \"acceptable\"" $ do
      get "/whitelist/acceptable" `shouldRespondWith` 200
    it "should return 403 for \"unacceptable\"" $ do
      get "/whitelist/unacceptable" `shouldRespondWith` 403

  describe "dynamic get" $ do
    it "should return JSON for False" $ do
      get "/dynamic/False" `shouldRespondWith` "15"
        { matchHeaders =
            [ "Content-Type" <:> "application/json"
            ]
        }
    it "should return plaintext for True" $ do
      get "/dynamic/True" `shouldRespondWith` "hello"
        { matchHeaders =
            [ "Content-Type" <:> "text/plain;charset=utf-8"
            ]
        }

