{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, RecordWildCards, TypeApplications,
             TypeFamilies, DataKinds, KindSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module FSSizes where

import Control.Concurrent.Async (forConcurrently, async, waitCatch)
import Control.Exception (SomeException, IOException, catch, handle)
import Control.Monad.Reader
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.Text
import Data.Word (Word64)
import Database.SQLite.Simple
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import qualified System.Directory as Dir
import System.Directory as Dir (doesDirectoryExist, doesFileExist, doesPathExist, getFileSize, listDirectory)
import Debug.Trace
import Control.Applicative
import Data.Int (Int64)
import Data.Either

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _) = error "fromRight: Left value"

pathInfoTable :: String
pathInfoTable = "fssizes"

type PathInfoSize = Either IOException Int64

data PathType = Unknown | File | Dir deriving (Enum, Show)

data PathInfo
  = PathInfo
    { pathInfoPath :: AbsoluteFilePath
    , pathInfoType :: PathType
    , pathInfoSize :: PathInfoSize
    }

newtype AbsoluteFilePath =
  AbsoluteFilePath { unAbsoluteFilePath :: FilePath } deriving Show

instance ToRow PathInfo where
  toRow PathInfo{..} =
    [ SQLText $ pack $ unAbsoluteFilePath pathInfoPath
    , SQLInteger (fromIntegral (fromEnum pathInfoType))
    , either (SQLText . pack .show) (const SQLNull) pathInfoSize
    , either (const SQLNull) (SQLInteger . fromIntegral) pathInfoSize
    ]

instance FromField PathType where
  fromField (fieldData -> SQLInteger n) = Ok $ toEnum $ fromIntegral n

insertPathInfo :: Connection -> PathInfo -> IO PathInfoSize
insertPathInfo conn pathInfo = do
  execute conn (Query $ pack $ "INSERT INTO " ++ pathInfoTable ++ " VALUES (?,?,?,?)") pathInfo
  pure $ pathInfoSize pathInfo

insertPathInfoError :: Connection -> (PathInfoSize -> PathInfo) -> IOException -> IO PathInfoSize
insertPathInfoError conn pathInfo e = insertPathInfo conn $ pathInfo $ Left e

instance ToField IOException where
  toField = toField . show

makeAbsolute :: FilePath -> IO AbsoluteFilePath
makeAbsolute = fmap AbsoluteFilePath . Dir.makeAbsolute

getFileSize :: FilePath -> ReaderT Connection IO PathInfoSize
getFileSize fp = do
  conn <- ask
  filePath <- liftIO $ makeAbsolute fp
  fileExists <- liftIO $ doesFileExist (unAbsoluteFilePath filePath)
  dirExists <- liftIO $ doesDirectoryExist (unAbsoluteFilePath filePath)
  let pathInfo = PathInfo filePath
  if | fileExists -> do
         let pathInfo' = pathInfo File
         liftIO $ handle (insertPathInfoError conn pathInfo') $ do
                           fileSize <- Dir.getFileSize (unAbsoluteFilePath filePath)
                           when (fileSize < 0) $ liftIO $ print fileSize
                           liftIO $ insertPathInfo conn $ pathInfo' $ Right $ fromIntegral fileSize
     | dirExists -> do
         let pathInfo' = pathInfo Dir
         liftIO $ handle (insertPathInfoError conn pathInfo') $ do
                           children <- liftIO $ listDirectory $ unAbsoluteFilePath filePath
                           size <- liftIO $ sum . rights <$> forConcurrently children (flip runReaderT conn . FSSizes.getFileSize . ((unAbsoluteFilePath filePath ++ "/") ++))
                           -- let selectStmt = Query $ pack $ "SELECT size FROM " ++ pathInfoTable ++ " WHERE size > 0"
                           -- size <- fold_ conn selectStmt (0 :: Int64) (\size (Only size') -> pure $ size + size')
                           insertPathInfo conn $ pathInfo' $ Right size
     | otherwise -> liftIO $ insertPathInfo conn (pathInfo Unknown $ Left $ userError "Unknown file type")

main :: IO ()
main = withConnection "fssizes.db" $ \conn -> do
  execute_ conn "DROP TABLE IF EXISTS fssizes"
  let createTableStmt = Query $ pack $ "CREATE TABLE IF NOT EXISTS " ++ pathInfoTable ++ " (path TEXT PRIMARY KEY, type INTEGER NOT NULL, error TEXT, size INTEGER)"
  execute_ conn createTableStmt
  let start = "."
  --let start = "./.git/objects/95"
  --let start = "/"
  flip runReaderT conn $ FSSizes.getFileSize start
  results <- query_ @(Text, PathType, Maybe Text, Maybe Int) conn "SELECT * FROM fssizes"
  mapM_ print results
  -- execute_ conn createTableStmt
  -- execute_ conn "INSERT INTO test VALUES ('path2', 0, null, 1)"
  -- execute_ conn "INSERT INTO test VALUES ('path3', 0, 'BAD!', null)"
  -- x <- query_ @(String, Int, Maybe String, Maybe Int) conn "SELECT * FROM test WHERE size > 0"
  -- print x
  -- x <- query_ @(Only Int) conn "SELECT size FROM test WHERE size >= 0"
  -- print x
  pure ()
-- main

--FSSizes.getFileSize "/" >>= print . dirSize
-- Just 1 <|> Just 2
