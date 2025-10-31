{-# LANGUAGE OverloadedStrings #-}

module DatabaseConfig where

import Data.Aeson (FromJSON, parseJSON)
import Data.Aeson.Types (withObject, (.:))
import Data.ByteString.Char8 qualified as BS8 (pack)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query (Query))
import GHC.Int (Int64)
import Data.Aeson qualified as Json
import Control.Lens.Internal.CTypes (Word16)

data ColumnConfig = ColumnConfig
  { columnName :: String,
    dataType :: String,
    index :: Bool,
    source :: [String]
  }
  deriving (Show)

instance FromJSON ColumnConfig where
  parseJSON = withObject "ColumnConfig" $ \v ->
    ColumnConfig
      <$> v .: "name"
      <*> v .: "type"
      <*> v .: "index"
      <*> v .: "source"

data TableConfig = TableConfig
  { tableName :: String,
    columns :: [ColumnConfig]
  }
  deriving (Show)

instance FromJSON TableConfig where
  parseJSON = withObject "TableConfig" $ \v ->
    TableConfig
      <$> v .: "name"
      <*> v .: "columns"

data DatabaseConfig = DatabaseConfig
  { host :: String,
    port :: Word16,
    user :: String,
    password :: String,
    database :: String,
    tables :: [TableConfig]
  }
  deriving (Show)

instance FromJSON DatabaseConfig where
  parseJSON = withObject "DatabaseConfig" $ \v ->
    DatabaseConfig
      <$> v .: "host"
      <*> v .: "port"
      <*> v .: "user"
      <*> v .: "password"
      <*> v .: "database"
      <*> v .: "tables"

createDatabase :: DatabaseConfig -> Query
createDatabase config =
  Query
    ( BS8.pack
        ( "CREATE DATABASE "
            ++ database config
            ++ " WITH OWNER = "
            ++ user config
        )
    )

databaseExists :: DatabaseConfig -> Query
databaseExists config =
  Query (BS8.pack ("SELECT 1 FROM pg_database WHERE datname = '" ++ database config ++ "'"))

checkDatabaseExists :: Connection -> DatabaseConfig -> IO Bool
checkDatabaseExists conn config = do
  results <- query_ conn (databaseExists config) :: IO [Only Int]
  return (not (null results))

joinStrings :: String -> [String] -> String
joinStrings separator strings = concatMap (++ separator) (init strings) ++ last strings

migrateTable :: Connection -> TableConfig -> IO ()
migrateTable conn tableConfig = do
  let columnDefs = map (\col -> columnName col ++ " " ++ dataType col) (columns tableConfig)
      createTableQuery =
        "CREATE TABLE IF NOT EXISTS "
          ++ tableName tableConfig
          ++ " ("
          ++ concatMap (++ ", ") (init columnDefs)
          ++ last columnDefs
          ++ ");"
  _ <- execute_ conn (Query (BS8.pack createTableQuery))
  mapM_ (createIndex conn (tableName tableConfig)) (columns tableConfig)

createIndex :: Connection -> String -> ColumnConfig -> IO Int64
createIndex conn tableName colConfig =
  if index colConfig
    then
      let indexName = tableName ++ "_" ++ columnName colConfig ++ "_idx"
          createIndexQuery =
            "CREATE INDEX IF NOT EXISTS "
              ++ indexName
              ++ " ON "
              ++ tableName
              ++ " ("
              ++ columnName colConfig
              ++ ");"
       in execute_ conn (Query (BS8.pack createIndexQuery))
    else return 0

connectionInfo :: DatabaseConfig -> ConnectInfo
connectionInfo config =
  ConnectInfo
    { connectHost = host config,
      connectPort = port config,
      connectUser = user config,
      connectPassword = password config,
      connectDatabase = database config
    }

migrate :: DatabaseConfig -> ConnectInfo -> IO ()
migrate config connInfo = do
  conn <- connect connInfo
  exists <- checkDatabaseExists conn config
  if exists
    then putStrLn $ "Database " ++ database config ++ " already exists."
    else
      execute_ conn (createDatabase config)
        >> putStrLn ("Database " ++ database config ++ " created.")
  mapM_ (migrateTable conn) (tables config)
  close conn

data InsertParams = InsertParams
  { insertIntoTableName :: String,
    insertIntoColumnNames :: [String],
    insertValues :: [Json.Value]
  }
  deriving (Show)

fillWithPlaceHolders :: Int -> String
fillWithPlaceHolders n = concatMap (++ ", ") (replicate (n - 1) "?") ++ "?"

insert :: ConnectInfo -> InsertParams -> IO ()
insert connInfo params = do
  let queryStr =
        "INSERT INTO "
          ++ insertIntoTableName params
          ++ " ("
          ++ joinStrings ", " (insertIntoColumnNames params)
          ++ ") VALUES (" ++ fillWithPlaceHolders (length (insertValues params)) ++ ");"
  conn <-  connect connInfo
  _ <- execute conn (Query (BS8.pack queryStr)) (insertValues params)
  close conn
