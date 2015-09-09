{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, runStderrLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Monoid
import Data.Aeson (FromJSON (..),  Value (Object), (.:), (.:?), decode')
import Data.ByteString (ByteString)
import Data.String (fromString)
import Data.Text (Text)
import Data.Foldable (forM_)
import Database.Persist (
      Entity (..), selectList, insert_, insertBy
    )
import Database.Persist.Sql (runMigration, transactionSave)
import Database.Persist.Sqlite (SqlPersistT, runSqlConn, withSqliteConn)
import Network.HTTP.Client (Manager, managerResponseTimeout, withManager)
import Network.HTTP.Client.Conduit (HasHttpManager (getHttpManager))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>), takeBaseName)
import Text.Printf (printf)

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Set                   as S
import qualified Data.Text                  as T

import API (HasAPIConfig (..), postObjects)
import Model

data APIEnvironment = APIEnvironment {
      aeManager :: Manager
    , aeApiRoot :: String
    , aeApiKey  :: ByteString
    }

instance HasHttpManager APIEnvironment where
    getHttpManager = aeManager

instance HasAPIConfig APIEnvironment where
    getApiRoot = aeApiRoot
    getApiKey  = aeApiKey

data Picture = Picture {
      pId       :: Text
    , pTitle    :: Text
    , pOwner    :: Owner
    , pUrl      :: Text
    , pTags     :: [Text]
    } deriving Show

instance FromJSON Picture where
    parseJSON (Object o) = Picture <$> o .: "id"
                                   <*> o .: "title"
                                   <*> o .: "owner"
                                   <*> o .: "url"
                                   <*> o .: "tags"
    parseJSON _          = mzero

data Owner = Owner {
      oId       :: Text
    , oName     :: Maybe Text
    , oUsername :: Maybe Text
    } deriving Show

instance FromJSON Owner where
    parseJSON (Object o) = Owner <$> o .: "id"
                                 <*> o .:? "name"
                                 <*> o .:? "username"
    parseJSON _          = mzero

-- | Pushes every local image into the API end-point.
--
-- usage: flickr-push-to-api <api root> <api key>  sqlite db> <flickr dir> <tag>
-- where <api root> is the URL of end point of the API, <api key> the private
-- API key, <sqlite db> the database which will contains image meta-data, <dir>
-- the directory which contains the crawled images and <tag> the tag prefix 
-- which will be used to group the images and their tags.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [apiRoot, apiKey, sqliteFile, dir, mainTag] -> do
            let tagPrefix = T.pack mainTag <> ":"

            -- Creates a mapping between photo id and the corresponding
            -- filepaths (.jpg image et .json meta-data files).
            fs <- getDirectoryContents dir
            let localIds = S.fromList [ T.pack $ takeBaseName f
                                      | f <-  fs
                                      , head f /= '.' ]

            withHttpSqlite sqliteFile $ \manager -> do
                runMigration migrateFlickr

                apiIds <- (S.fromList . map (flickrPictureFlickrId . entityVal))
                          <$> selectList [] []

                let notInApisIds = S.difference localIds apiIds
                liftIO $ printf "%d of %d images not in the API\n"
                                (S.size notInApisIds) (S.size localIds)

                let env = APIEnvironment manager apiRoot (fromString apiKey)

                forM_ notInApisIds $ \flickrId -> do
                    let basePath = dir </> T.unpack flickrId
                        jsonPath = basePath <.> "json"
                        jpgPath  = basePath <.> "jpg"

                    liftIO $ printf "Inserting %s ...\n" jpgPath

                    Just Picture {..} <-
                        liftIO $! decode' <$> C.readFile jsonPath

                    -- Pushs to the API.
                    Just apiId <- flip runReaderT env $
                        postObjects (Just pUrl) jpgPath 
                                    (map (tagPrefix <>) pTags) False

                    -- Inserts the meta-data into the database.
                    let Owner {..} = pOwner
                    eOwner <- insertBy $ FlickrOwner pId oName oUsername
                    let ownerId = either entityKey id eOwner

                    insert_ $ FlickrPicture pId apiId pTitle ownerId pUrl pTags

                    transactionSave
        _ -> do
            putStrLn "usage: flickr-push-to-api <api root> <api key> \
                     \<sqlite db> <flickr dir> <tag>"
            putStrLn "where <api root> is the URL of end point of the API,"
            putStrLn "<api key> the private API key, <sqlite db> the database "
            putStrLn "which will contains image meta-datas, <dir> the "
            putStrLn "directory which contains the crawled images and <tag> "
            putStrLn "the tag prefix which will be used to group the images "
            putStrLn "and their tags."

-- | Runs the given action with an HTTP manager and a SQLite connection.
withHttpSqlite :: String -> (Manager -> SqlPersistT (LoggingT IO) a) -> IO a
withHttpSqlite sqliteFile action =
    withManager settings $ \manager ->
        runStderrLoggingT $
            withSqliteConn (T.pack sqliteFile) $ \conn ->
                runSqlConn (action manager) conn
  where
    settings = tlsManagerSettings {
                      managerResponseTimeout = Just maxBound
                    }
