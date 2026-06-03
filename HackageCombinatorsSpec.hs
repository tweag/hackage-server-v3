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
      :<|> "format" :> Capture "something" (WithFormat String "json") :> Get '[JSON] String
      :<|> "auth" :> HackageAuth :> Get '[JSON] ()
      :<|> NegotiableContent :> "negotiable" :> Capture "something" String :> Get '[PlainText, JSON] String

      -- get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}
spec :: Spec
spec = with (pure $ serveWithContext (Proxy @API) (hackageAuthHandler hackageRealm undefined :. EmptyContext)
    (undefined
      :<|> pure . unWithFormat
      :<|> const (pure ())
      :<|> pure
    )) $ do

  describe "format" $ do
    it "should match when capturing the format" $ do
      get "/format/hello.json" `shouldRespondWith` "hello"
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


main = hspec spec
