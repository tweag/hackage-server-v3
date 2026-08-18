{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}

module Hackage.Main
  ( main
  , mainImpl
  , Options(..)
  , connPool
  ) where

import Data.BlobStorage qualified as Blob
import Data.ByteString (StrictByteString)
import Data.Pool
import Data.Proxy
import Data.Text qualified as T
import Hackage.API.PackageDb
import Hackage.API.Type
import Hackage.ServerM
import Hackage.SyncAPI.Sync
import Hackage.SyncAPI.Type
import Hasql.Connection
import Hasql.Connection.Setting qualified as DB
import Hasql.Connection.Setting.Connection qualified as DB
import Network.HTTP.Client.TLS
import Network.Wai.Handler.Warp
import Options.Applicative
import Servant.API
import Servant.HackageCombinators.UserDomain (UserDomain(..))
import Servant.Server


main :: IO ()
main = mainImpl =<< execParser optionsParser


mainImpl :: Options -> IO ()
mainImpl opts = do
  client <- newTlsManager
  pool <- newPool $ connPool opts
  blobStore <- Blob.open $ optBlobStore opts
  app <-
    serverMToWai
      (Proxy @(
        NamedRoutes PackageDbApi :<|> NamedRoutes SyncApi
        ))
      (client
        :. UserDomain (optUserDomain opts)
        :. EmptyContext
      )
      (ServerCtx pool blobStore)
      $ packageDbServer :<|> syncServer
  run (optPort opts) app


data Options = Options
  { optDb :: DB.Connection
  , optBlobStore :: FilePath
  , optConnections :: Int
  , optPort :: Port
  , optUserDomain :: StrictByteString
  }


parseDb :: Parser DB.Connection
parseDb = fmap (DB.string . T.pack) $
  strOption $ mconcat
    [ long "db"
    , metavar "CONNECTION_STRING"
    , help "PostgreSQL connection string"
    ]


parseBlobStore :: Parser FilePath
parseBlobStore =
  strOption $ mconcat
    [ long "blob-store"
    , metavar "FILE"
    , help "Path to a hackage server blob store. This directory will be created if it doesn't already exist."
    ]


parseConnections :: Parser Int
parseConnections =
  option auto $ mconcat
    [ long "num-connections"
    , metavar "INTEGER"
    , help "The max number of connections to the database to keep open."
    , value 100
    ]


parseOptions :: Parser Options
parseOptions = do
  optDb <- parseDb
  optBlobStore <- parseBlobStore
  optConnections <- parseConnections
  optPort <- option auto $
    mconcat
      [ long "port"
      , metavar "PORT"
      , help "The port to serve hackage server on."
      , value 8000
      ]
  optUserDomain <- option auto $
    mconcat
      [ long "user-content-uri"
      , metavar "URI"
      , help "The domain to serve user content from."
      ]
  pure $ Options {..}


connPool :: Options -> PoolConfig Connection
connPool opts =
  setNumStripes Nothing $
    defaultPoolConfig
      (acquire (pure $ DB.connection $ optDb opts) >>= either (error . show) pure)
      release
      30
      (optConnections opts)


optionsParser :: ParserInfo Options
optionsParser = info (parseOptions <**> helper) $ mconcat
  [ fullDesc
  , progDesc "Hackage server v3"
  , header "hackage-server-v3"
  ]

