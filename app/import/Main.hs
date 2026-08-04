module Main where

import Data.Text qualified as T
import Hackage.SetupDB (setupDB)
import Hackage.Utils (withConn)
import Hasql.Connection.Setting qualified as DB
import Hasql.Connection.Setting.Connection qualified as DB
import Mirror
import Options.Applicative


data Command
  = MakeDb
  | BackfillPackages FilePath
  | BackfillBlobstore FilePath
  deriving stock (Show, Eq)


data Options = Options
  { optDb :: T.Text
  , optCommand :: Command
  } deriving stock (Show, Eq)


makeDbCmd :: Parser Command
makeDbCmd = pure MakeDb


backfillPackagesCmd :: Parser Command
backfillPackagesCmd =
  fmap BackfillPackages $ strArgument $ mconcat
    [ metavar "ACID_STATE_DIR"
    , help "Path to the hackage-v2 acid-state directory"
    ]


backfillBlobstoreCmd :: Parser Command
backfillBlobstoreCmd =
  fmap BackfillBlobstore $ strArgument $ mconcat
    [ metavar "BLOBSTORE_DIR"
    , help "Path to the hackage-v2 blobstore directory"
    ]


commandParser :: Parser Command
commandParser =
  subparser $ mconcat
    [ command "make-db"
        $ info makeDbCmd
        $ progDesc "Initialize the database schema"
    , command "backfill-packages"
        $ info backfillPackagesCmd
        $ progDesc "Backfill package data into the database"
    , command "backfill-blobstore"
        $ info backfillBlobstoreCmd
        $ progDesc "Backfill blobs into the blobstore"
    ]


dbOption :: Parser T.Text
dbOption = fmap T.pack $
  strOption $ mconcat
    [ long "db"
    , metavar "CONNECTION_STRING"
    , help "PostgreSQL connection string"
    ]


optionsParser :: Parser Options
optionsParser = Options <$> dbOption <*> commandParser


opts :: ParserInfo Options
opts = info (optionsParser <**> helper) $ mconcat
  [ fullDesc
  , progDesc "Import tool for hackage-server v2"
  , header "hackage-server-v2-import - import and backfill Hackage data"
  ]


main :: IO ()
main = do
  Options{optDb, optCommand} <- execParser opts
  withConn (pure $ DB.connection $ DB.string optDb) $ \conn ->
    case optCommand of
      MakeDb -> setupDB conn
      BackfillPackages acidDir ->
        backfillPackageDB conn acidDir
      BackfillBlobstore blobDir ->
        backfillTarIndex conn blobDir

