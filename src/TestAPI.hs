{-# LANGUAGE OverloadedStrings #-}

module TestAPI where

import Data.BlobStorage qualified as Blob
import Servant.Server.Generic (AsServerT)
import GHC.Generics
import Hackage.Types.PrimaryKey
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
import Servant.HackageAuth (hackageRealm, newPasswdHash, PasswdPlain (..))
import Servant.HackageCombinators
import Servant.Server
import Hackage.Utils
import Hackage.Types
import Rel8 hiding (run)
import Rel8.Expr.Time (now)

import Data.Text qualified as T
import Hackage.Schemas.Users


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
      100



main :: IO ()
main = do
  client <- newTlsManager
  pool <- newPool connPool
  blobStore <- Blob.open "../hackage-server/state/blobs"
  app <-
    runServerM
      (Proxy @(
        NamedRoutes PackagesHtmlAPI
        :<|> "bootstrap" :> NamedRoutes BootstrapAPI
        ))
      (client
        :. hackageAuthHandler hackageRealm pool
        :. EmptyContext
      )
      (ServerCtx pool blobStore)
      $ packagesHtmlServer
        :<|> bootstrap
  run 8000 app


data BootstrapAPI mode = BootstrapAPI
  { bootstrapNewUser :: mode
      :- "users" :> Capture "userid" String :> "new" :> Capture "password" String :> Get '[JSON] [UserId]
  , bootstrapPromote :: mode
      :- HackageAuth :> "users" :> "promote" :> Get '[JSON] ()
  }
  deriving stock Generic

bootstrap :: BootstrapAPI (AsServerT ServerM)
bootstrap = BootstrapAPI
  { bootstrapNewUser = \user pass ->
      liftDB $ doInsert $ Insert
        { into = usersSchema
        , rows = values
            [ UsersRow
                { userId = unsafeDefault
                , userName = lit $ T.pack user
                , userEmail =  lit Nothing
                , userRealName =  lit Nothing
                , userAuth =  lit $ newPasswdHash hackageRealm (T.pack user) $ PasswdPlain pass
                , userStatus = lit Enabled
                , userAdminNotes = mempty
                , userCreatedTime = now
                }
            ]
        , onConflict = Abort
        , returning = Returning userId
        }
  , bootstrapPromote = \user ->
      liftDB $ doInsert_ $ Insert
        { into = userRolesSchema
        , rows = values
            [ UserRoleRow
                { userRoleId = newPrimaryKey
                , userRoleUserId = lit user
                , userRoleRole = lit Admin
                , userRoleAssignedTime = now
                }
            ]
        , onConflict = Abort
        , returning = NoReturning
        }
  }

