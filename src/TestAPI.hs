{-# LANGUAGE OverloadedStrings #-}

module TestAPI where

import Data.Map qualified as M
import Hackage.Features.Upload
import Servant.HackageAuth (hackageRealm)
import Data.Proxy
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Servant.HackageCombinators
import Hasql.Connection
import Control.Exception (bracket)
import qualified Hasql.Connection.Setting as DB
import qualified Hasql.Connection.Setting.Connection as DB
import Hackage.Types
import Servant.EDE
import Network.HTTP.Client.TLS


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


main :: IO ()
main = do
  eerrs <-
    loadTemplates (Proxy @API) [] "templates" $ do
      client <- newTlsManager
      withConn (pure $ DB.connection $ DB.string "postgresql://sandy@/sandy") $ \conn -> do
        run 8000 $
          serveWithContext
            (Proxy @API)
            (client
              :. hackageAuthHandler hackageRealm conn
              :. EmptyContext
            ) $ pure
          :<|>
            const
                  (WithCacheControl [Public, NoCache] $ pure $ TrusteesObject $ M.fromList [ (UserId 0, "isovector") ])
          :<|> NotYetPorted
  either print pure eerrs
