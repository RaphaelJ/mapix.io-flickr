{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Reader (runReaderT)
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.String (fromString)
import qualified Data.Text as T
import Database.Persist (
      Entity (..), (==.), selectKeysList, selectList, insert_
    )
import Database.Persist.Sql (runMigration)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.Conduit (HasHttpManager (getHttpManager))
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>), takeBaseName)

import API (HasAPIConfig (..), postImage)
import Model
import Util (withHttpSqlite)

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

-- | Pushes every local image into the API end-point.
--
-- usage: flickr-push-to-api <api root> <api key> <sqlite db> <dir> <tag>
-- where <api root> is the URL of end-point of the API, <api key> the private
-- API key, <sqlite db> the database which contains the image mapping, <dir> the
-- directory which contains the images and <tag> the tag which will be used to
-- group the images and to prefix their tags.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [apiRoot, apiKey, sqliteFile, dir, tag] -> do
            -- Creates a mapping between photo id and the corresponding
            -- filepath.
            fs <- getDirectoryContents dir
            let photoIdToFile = M.fromList [ (T.pack filename, dir </> f)
                                           | f <- fs
                                           , let filename = takeBaseName f ]

            withHttpSqlite sqliteFile $ \manager -> do
                runMigration migrateFlickr

                let env       = APIEnvironment manager apiRoot
                                               (fromString apiKey)
                    tagTxt    = T.pack tag
                    tagPrefix = tag ++ ":"

                pics <- selectList [] []
                forM_ pics $ \(Entity picId pic) -> do
                    let photoId   = flickrImagePhotoId pic
                        Just path = photoId `M.lookup` photoIdToFile

                    tags <-     map (T.pack . (tagPrefix ++) . show)
                            <$> selectKeysList [ FlickrImageTagImage ==. picId ]
                                               []

                    Just code <- runReaderT (postImage path (tagTxt : tags)) env

                    insert_ (ApiImage code picId)
        _ -> do
            putStrLn "usage: flickr-push-to-api <api root> <api key> \
                     \<sqlite db> <dir> <tag>"
            putStrLn "where <api root> is the URL of end point of the API,"
            putStrLn "<api key> the private API key, <sqlite db> the database"
            putStrLn "which contains the image mapping, <dir> the directory"
            putStrLn "which contains the images and <tag> the tag which will"
            putStrLn "be used to group the images and to prefix their tags."
