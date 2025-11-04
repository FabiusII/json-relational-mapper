{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson as Json
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (lookup)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Yaml as Yaml
import Database.PostgreSQL.Simple (ConnectInfo)
import DatabaseConfig
import Prelude hiding (lookup)
import System.Environment (getArgs)

getFromSourcePath :: Json.Value -> [String] -> [Json.Value]
getFromSourcePath v [] = [v]
getFromSourcePath (Json.Object o) (key : rest) = case lookup (fromString key) o of
  Just v -> getFromSourcePath v rest
  Nothing -> [Json.Null]
getFromSourcePath (Json.Array as) path = concatMap (\a -> getFromSourcePath a path) as
getFromSourcePath _ _ = [Json.Null]

produceInsertParams :: [TableSetter] -> Json.Value -> [InsertParams]
produceInsertParams tableSetter v =
  concatMap
    ( \t ->
        let valsPerSourcePath = map (getFromSourcePath v) (sourcePaths t)
            cartesianProduct = sequence valsPerSourcePath
         in map
              ( \vs ->
                  InsertParams
                    { insertIntoTableName = table t,
                      insertIntoColumnNames = cols t,
                      insertValues = vs
                    }
              )
              cartesianProduct
    )
    tableSetter

insertLineByLine :: ConnectInfo -> [String] -> [TableSetter] -> IO ()
insertLineByLine connInfo (l : ls) setters = do
  case Json.decode (encodeUtf8 (LT.pack l)) :: Maybe Json.Value of
    Just v -> mapM_ (insert connInfo) (produceInsertParams setters v)
    Nothing -> putStrLn $ "Failed to parse line: " ++ l
  insertLineByLine connInfo ls setters
insertLineByLine _ [] _ = pure ()

type SourcePath = [String]

data TableSetter = TableSetter
  { table :: String,
    cols :: [String],
    sourcePaths :: [SourcePath]
  }

createSetters :: DatabaseConfig.DatabaseConfig -> [TableSetter]
createSetters config =
  map
    ( \tableConfig ->
        TableSetter
          { table = DatabaseConfig.tableName tableConfig,
            cols = map DatabaseConfig.columnName (DatabaseConfig.columns tableConfig),
            sourcePaths = map DatabaseConfig.source (DatabaseConfig.columns tableConfig)
          }
    )
    (DatabaseConfig.tables config)

getDatabaseConfigFilePath :: [String] -> Maybe String
getDatabaseConfigFilePath ("--config" : path : _) = Just path
getDatabaseConfigFilePath (_ : args) = getDatabaseConfigFilePath args
getDatabaseConfigFilePath _ = Nothing

getDataFilePath :: [String] -> Maybe String
getDataFilePath ("--data" : path : _) = Just path
getDataFilePath (_ : args) = getDataFilePath args
getDataFilePath _ = Nothing

runMainWithParams :: String -> String -> IO ()
runMainWithParams dbConfigPath dataFilePath = do
  config <- Yaml.decodeFileThrow dbConfigPath
  print config
  let connInfo = connectionInfo config
      setters = createSetters config
  migrate config connInfo
  putStr "Migration completed.\n"
  content <- readFile dataFilePath
  insertLineByLine connInfo (lines content) setters

main :: IO ()
main = do
  args <- getArgs
  dbConfigPath <- case getDatabaseConfigFilePath args of
    Just path -> return path
    Nothing -> error "Database config file path not provided. Use --config <path>."
  dataFilePath <- case getDataFilePath args of
    Just path -> return path
    Nothing -> error "Data file path not provided. Use --data <path>."
  runMainWithParams dbConfigPath dataFilePath
