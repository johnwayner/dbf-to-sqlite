module Main where

import System.Environment
import System.FilePath.Posix
import DBase
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as LIST
import qualified Data.Char as CHAR
import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Database.HDBC as DB
import qualified Codec.Archive.Zip as ZIP

main :: IO ()
main = do
  zipPaths <- getArgs
  extractZips zipPaths

extractZips :: [FilePath] -> IO ()
extractZips [] = return ()
extractZips (zipPath:remainingZipPaths) = do
  rawArchive <- BL.readFile zipPath
  putStrLn $ "Processing " ++ zipPath
  exportTables (filter (LIST.isSuffixOf "dbf" . (map CHAR.toLower) . ZIP.eRelativePath) (ZIP.zEntries $ ZIP.toArchive rawArchive)) $ toTableName zipPath ++ ".sqlite"
  extractZips remainingZipPaths


exportTables :: [ZIP.Entry] -> FilePath -> IO ()
exportTables [] _ = return ()
exportTables (dbaseEntry:otherPaths) sqliteFilePath = do
  readTableAndAddToDb dbaseEntry sqliteFilePath
  exportTables otherPaths sqliteFilePath


toTableName :: FilePath -> String
toTableName fp = takeWhile (/='.') $ takeBaseName fp

readTableAndAddToDb :: ZIP.Entry -> FilePath -> IO ()
readTableAndAddToDb dbaseEntry sqliteFilePath = do
  let dbaseData = ZIP.fromEntry dbaseEntry
      table = parseDBaseTable dbaseData (toTableName $ ZIP.eRelativePath dbaseEntry)
    in addTableToDB table sqliteFilePath

addTableToDB :: Table -> FilePath -> IO ()
addTableToDB table dbFilePath = do
  conn <- connectSqlite3 dbFilePath
  dropResult <- DB.run conn (genDropTableSQL table) []
  result <- DB.run conn (genCreateTableSQL table) []
  insertStatement <- DB.prepare conn $ genInsertSQL table
  insertResult <- DB.executeMany insertStatement $ genAllValues $ records table
  DB.commit conn
  DB.disconnect conn
  putStrLn $ "Table: " ++ tableName table
  putStrLn $ "Drop Table: " ++ if dropResult == 0 then "OK" else "Failed"
  putStrLn $ "Create Table: " ++ if result == 0 then "OK" else "Failed"
  putStrLn $ "Insert Rows (" ++ show (length (records table)) ++ "): " ++ if insertResult == () then "OK" else "Failed"
