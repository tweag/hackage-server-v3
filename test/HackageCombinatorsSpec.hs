{-# LANGUAGE OverloadedStrings #-}

module HackageCombinatorsSpec where

import Data.Proxy (Proxy(..))
import Test.Hspec
import Test.Hspec.Wai
import Servant.API
import Servant.Server
import Servant.HackageCombinators
import Servant.HackageAuth (hackageRealm)

type API = "redirect" :> PermanentRedirect
      :<|> "format" :> CaptureExt "something" String "json" :> Get '[JSON] String
      :<|> "format" :> CaptureExt "something" String "tar.gz" :> Get '[JSON] String
      :<|> "auth" :> HackageAuth :> Get '[JSON] ()
      :<|> "cache" :> CacheControl :> Get '[JSON] ()
      :<|> "user" :> UserDomain :> Get '[JSON] ()
      :<|> NegotiableContent :> "negotiable" :> Capture "something" String :> Get '[PlainText, JSON] String


spec :: Spec
spec =
  with (pure $
    serveWithContext
      (Proxy @API)
      (UserDomain "my.user.domain"
        :. hackageAuthHandler hackageRealm undefined
        :. EmptyContext
      )
      (undefined
        :<|> pure
        :<|> pure
        :<|> const (pure ())
        :<|> WithCacheControl [MaxAge 1234, SharedMaxAge 4321] (pure ())
        :<|> pure ()
        :<|> pure
      )) $ do

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
    it "should dispatch txt based on extension" $ do
      get "/negotiable/hello.txt" `shouldRespondWith` "hello"
    it "should dispatch json based on extension" $ do
      get "/negotiable/hello.json" `shouldRespondWith` "\"hello\""
    it "should not dispatch html based on extension" $ do
      get "/negotiable/hello.html" `shouldRespondWith` 406

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

