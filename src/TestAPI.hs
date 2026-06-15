{-# LANGUAGE OverloadedStrings #-}

module TestAPI where

import Control.Exception (bracket)
import Data.Proxy
import Hackage.API.PackagesHTML
import Hackage.ServerM
import Hackage.Types
import Hasql.Connection
import Hasql.Connection.Setting qualified as DB
import Hasql.Connection.Setting.Connection qualified as DB
import Network.HTTP.Client.TLS
import Network.Wai.Handler.Warp
import Servant.API
import Servant.EDE
import Servant.HackageAuth (hackageRealm)
import Servant.HackageCombinators
import Servant.Server


type API = "test"
              :> HackageAuth
              :> Get '[JSON] UserId
      :<|> NegotiableContent
              :> "test"
              :> Capture "ok" String
              :> CacheControl
              :> Get '[JSON, HTML ] TrusteesObject
      :<|> NotYetPorted


withConn :: [DB.Setting] -> (Connection -> IO a) ->  IO a
withConn ss = bracket (acquire ss >>= either (error . show) pure) release


mkConn :: (Connection -> IO r) -> IO r
mkConn = withConn $ pure $ DB.connection $ DB.string "postgresql://sandy@/sandy"


main :: IO ()
main = do
  client <- newTlsManager
  mkConn $ \conn -> do
    app <-
      runServerM
        (Proxy @(NamedRoutes PackagesHtmlAPI :<|> NotYetPorted))
        (client
          :. hackageAuthHandler hackageRealm conn
          :. EmptyContext
        ) undefined $ packagesHtmlServer :<|> NotYetPorted
    run 8000 app

