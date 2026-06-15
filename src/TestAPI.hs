{-# LANGUAGE OverloadedStrings #-}

module TestAPI where

import Data.Pool
import Control.Exception (bracket)
import Data.Proxy
import Hackage.API.PackagesHTML
import Hackage.ServerM
import Hasql.Connection
import Hasql.Connection.Setting qualified as DB
import Hasql.Connection.Setting.Connection qualified as DB
import Network.HTTP.Client.TLS
import Network.Wai.Handler.Warp
import Servant.API
import Servant.HackageAuth (hackageRealm)
import Servant.HackageCombinators
import Servant.Server


withConn :: [DB.Setting] -> (Connection -> IO a) ->  IO a
withConn ss = bracket (acquire ss >>= either (error . show) pure) release


mkConn :: (Connection -> IO r) -> IO r
mkConn = withConn (pure $ DB.connection $ DB.string "postgresql://sandy@/sandy")

connPool :: PoolConfig Connection
connPool =
  setNumStripes Nothing $
    defaultPoolConfig
      (acquire (pure $ DB.connection $ DB.string "postgresql://sandy@/sandy") >>= either (error . show) pure)
      release
      30
      10

main :: IO ()
main = do
  client <- newTlsManager
  pool <- newPool connPool
  app <-
    runServerM
      (Proxy @(NamedRoutes PackagesHtmlAPI :<|> NotYetPorted))
      (client
        :. hackageAuthHandler hackageRealm pool
        :. EmptyContext
      )
      (ServerCtx pool)
      $ packagesHtmlServer
        :<|> NotYetPorted
  run 8000 app

