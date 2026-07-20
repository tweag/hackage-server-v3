module Main where

import Options.Applicative
import SetupDB qualified
import Mirror


data Command
  = MakeDb
  | BackfillPackages FilePath
  | BackfillBlobstore FilePath
  deriving stock (Show, Eq)


data Options = Options
  { optCommand :: Command
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



optionsParser :: Parser Options
optionsParser = Options <$> commandParser


opts :: ParserInfo Options
opts = info (optionsParser <**> helper) $ mconcat
  [ fullDesc
  , progDesc "Import tool for hackage-server v2"
  , header "hackage-server-v2-import - import and backfill Hackage data"
  ]


main :: IO ()
main = do
  Options{optCommand} <- execParser opts
  case optCommand of
    MakeDb -> SetupDB.main
    BackfillPackages acidDir -> backfillPackageDB acidDir
    BackfillBlobstore blobDir -> backfillTarIndex blobDir

