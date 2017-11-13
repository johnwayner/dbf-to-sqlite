module DBase
  ( Table(..)
  , Record
  , Column(..)
  , ColumnDef(..)
  , DataType
  , parseDBaseTable
  , genCreateTableSQL
  , genInsertSQL
  , genAllValues
  ) where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as Char8
import Database.HDBC
import Data.List


data DataType
  = Character
  | Date
  | FloatingPoint
  | Numeric
  | Logical
  | Memo
  deriving (Show)

data ColumnDef = ColumnDef
  { colName :: String
  , dataType :: DataType
  , len :: Int
  , decimalCount :: Integer
  } deriving (Show)

data Column
  = CharacterData String
  | DateData String
  | NumericData Integer
  | FloatingPointData Double
  | LogicalData Bool
  | MemoData String
  | NullData
  deriving (Show)

type Record = [Column]

data Header = Header
  { lastUpdate :: String
  , numRecords :: Integer
  , bytesPerRecord :: Integer
  } deriving (Show)

data Table = Table
  { tableName :: String
  , header :: Header
  , columnDefs :: [ColumnDef]
  , records :: [Record]
  } deriving (Show)

parseTableHeader :: Get Header
parseTableHeader = do
  _ <- getWord8
  lstUptYear1900 <- getWord8
  lstUptMonth <- getWord8
  lstUptDay <- getWord8
  numRecs <- getWord32le
  _ <- getWord16le
  bpr <- getWord16le
  _ <- getByteString 20
  return
    Header
    { lastUpdate = show (toInteger lstUptYear1900 + 1900) ++ "-" ++ show lstUptMonth ++ "-" ++ show lstUptDay
    , numRecords = toInteger numRecs
    , bytesPerRecord = toInteger bpr
    }

willBeEndOfFields :: Get Bool
willBeEndOfFields = do
  nextByte <- getWord8
  case nextByte of
    0x0d -> return True
    _ -> return False

isNull :: Char -> Bool
isNull '\0' = True
isNull _ = False

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace _ = False

parseDouble :: String -> Double
parseDouble value = fst $ head (reads value :: [(Double, String)])

parseInteger :: String -> Integer
parseInteger value = fst $ head (reads value :: [(Integer, String)])

parseColumnDef :: Get ColumnDef
parseColumnDef = do
  name <- getByteString 11
  dataTypeBS <- getByteString 1
  _ <- getByteString 4
  len_ <- getWord8
  decimalCount_ <- getWord8
  _ <- getByteString 14
  return
    ColumnDef
    { colName = filter (not . isNull) $ Char8.unpack name
    , dataType =
        case Char8.unpack dataTypeBS of
          "C" -> Character
          "D" -> Date
          "F" -> FloatingPoint
          "N" -> if decimalCount_ > 0 then FloatingPoint else Numeric
          "L" -> Logical
          "M" -> Memo
          ___ -> Character
    , len = fromIntegral len_
    , decimalCount = toInteger decimalCount_
    }

parseColumnDefs :: Get [ColumnDef]
parseColumnDefs = do
  endOfDefs <- lookAhead willBeEndOfFields
  if endOfDefs
    then do
      _marker <- getWord8
      return []
    else do
      def <- parseColumnDef
      rest <- parseColumnDefs
      return (def : rest)

parseColumn :: DataType -> Int -> Get Column
parseColumn dataType_ len_ = do
  s <- getByteString len_
  return $
    let value = Char8.unpack s
    in if all isSpace value
         then NullData
         else case dataType_ of
                FloatingPoint -> FloatingPointData $ parseDouble value
                Numeric -> NumericData $ parseInteger value
                Logical ->
                  case value of
                    "Y" -> LogicalData True
                    "y" -> LogicalData True
                    "T" -> LogicalData True
                    "t" -> LogicalData True
                    ___ -> LogicalData False
                _ -> CharacterData value

parseRecord :: [ColumnDef] -> Get Record
parseRecord [] = return []
parseRecord (columnDef:cds) = do
  column <- parseColumn (dataType columnDef) (len columnDef)
  rest <- parseRecord cds
  return (column : rest)

parseRecords :: Integer -> [ColumnDef] -> Get [Record]
parseRecords 0 _ = return []
parseRecords recordsRemaining columns = do
  deleteIndicator <- getByteString 1
  record <- parseRecord columns
  rest <- parseRecords (recordsRemaining - 1) columns
  if Char8.unpack deleteIndicator == "*"
    then return rest
    else return (record : rest)

parseDBaseTable :: B.ByteString -> String -> Table
parseDBaseTable byteString name = runGet parseTable byteString
  where
    parseTable = do
      h <- parseTableHeader
      cds <- parseColumnDefs
      rs <- parseRecords (numRecords h) cds
      return Table {tableName = name, header = h, columnDefs = cds, records = rs}

genCreateColumnSQL :: ColumnDef -> String
genCreateColumnSQL col =
  " `"
  ++ colName col
  ++ "` "
  ++ case dataType col of
       Character -> "TEXT"
       Numeric -> "INT"
       FloatingPoint -> "REAL"
       _ -> ""

genCreateTableSQL :: Table -> String
genCreateTableSQL table =
  "CREATE TABLE "
  ++ tableName table
  ++ "(\n"
  ++ intercalate ",\n" (map genCreateColumnSQL $ columnDefs table)
  ++ "\n); "


genInsertSQL :: Table -> String
genInsertSQL table =
  "INSERT INTO "
  ++ tableName table
  ++ " VALUES ("
  ++ intercalate "," parameterSlots
  ++ ")"
  where
    numCols = length $ columnDefs table
    parameterSlots = replicate numCols "?"

genAllValues :: [Record] -> [[SqlValue]]
genAllValues [] = []
genAllValues (record:remainingRecords) = (genSqlValues record : genAllValues remainingRecords)

genSqlValues :: [Column] -> [SqlValue]
genSqlValues [] = []
genSqlValues (col:remainingCols) = (sqlValue : genSqlValues remainingCols)
  where sqlValue = case col of
          NullData -> SqlNull
          CharacterData v -> toSql v
          NumericData v -> toSql v
          FloatingPointData v -> toSql v
          LogicalData v -> toSql v
          DateData v -> toSql v
          MemoData v -> toSql v
