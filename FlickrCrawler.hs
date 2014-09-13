{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

import Control.Applicative
import qualified Control.Exception as E
import Control.Lens hiding ((<.>))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Lazy as B
import Data.List (find)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>), takeExtension)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Random
import Text.Printf

import Database.Persist (
      entityKey, entityVal, insert_, insertBy, insertUnique, selectList
    )
import Database.Persist.Sql (runMigration)

import qualified Flickr.API as F
import qualified Flickr.Photos as F

import System.Random.Shuffle (shuffle')

import Network.Wreq (getWith, defaults, manager, responseBody)

import Model
import Util (withHttpSqlite)

data Photo = Photo {
      pId       :: Text
    , pTitle    :: Text
    , pUrl      :: Text
    , pTags     :: [Text]
    , pImageUrl :: String
    , pImageExt :: String
    , pWidth    :: Int
    , pHeight   :: Int
    } deriving Show

pageSize :: Int
pageSize = 10

-- | Fetches public images using the Flickr API.
--
-- usage: flickr-crawler <dict file> <sqlite db> <dest dir>
-- where <dict file> if a file containing keywords to search, <sqlite db> the
-- SQLite database where meta-data will be stored and <dest dir> the directory
-- where images will be stored.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [dictFile, sqliteFile, dstDir] -> do
            dict <- lines <$> liftIO (readFile dictFile)

            withHttpSqlite sqliteFile $ \httpMan -> do
                runMigration migrateFlickr

                -- Retrieve already fetched images from the database.
                skipPics <- selectList [] []

                let skipIds = map (T.unpack . flickrImagePhotoId . entityVal)
                                  skipPics

                pics <- liftIO $ photos (mkStdGen 0) dict skipIds

                forM_ (zip [(length skipIds + 1)..] pics) $ \(i, pic) -> do
                    mKey <- addPhoto httpMan dstDir pic

                    case mKey of
                        Just key ->
                            liftIO $ printf "%d\t%s\t(key : %s)\n" i
                                            (T.unpack (pUrl pic)) (show key)
                        Nothing  -> return ()
        _            -> do
            putStrLn "usage: loader-flickr <dict file> <sqlite db> <dest dir>"
            putStrLn "where <dict file> if a file containing keywords to"
            putStrLn "search, <sqlite db> the SQLite database where meta-data"
            putStrLn "will be stored and <dest dir> the directory where images"
            putStrLn "will be stored."
  where
    addPhoto httpMan dstDir Photo { .. } = do
        let path = dstDir </> T.unpack pId <.> pImageExt

        let opts = defaults & manager .~ Right httpMan
        bs <- liftIO $ getWith opts pImageUrl

        runMaybeT $ do
            key <- MaybeT $ insertUnique $ FlickrImage pId pTitle pUrl pWidth
                                                       pHeight

            liftIO $ B.writeFile path (bs ^. responseBody)

            lift $ mapM_ (insertTag key) pTags

            return key

    insertTag picKey tag = do
        tagKey <- either entityKey id <$> insertBy (FlickrTag tag)
        insert_ $ FlickrImageTag picKey tagKey

-- | Returns an infinite list of FlickR photos taken from random searches using
-- the given dictionnary and random generator. Skips images from the given list.
photos :: RandomGen g => g -> [String] -> [F.PhotoID] -> IO [Photo]
photos gen dict skip =
    go dict' [] 1 (S.fromList skip)
  where
    filters = F.nullSearchConstraints {
          F.s_license = Just ["4"] -- Attribution License
        }

    dict' = shuffle' dict (length dict) gen

    -- Fetches the first page of every word, then tries the second page for the
    -- same set of words.
    go []       []  _    _    = return []
    go []       ws' page imgs = go (reverse ws') [] (page + 1) imgs
    go (w : ws) ws' page imgs = unsafeInterleaveIO $ do
        (ctx, pics) <- F.flick $ F.withPageSize pageSize $
            let filters' = filters { F.s_text = Just w }
            in F.pagedCall (Just page) $ F.search Nothing filters' []

        (res, imgs') <- goPix pics imgs

        let Just nPages = F.photoCtxtPages ctx

        -- Goes to the next word, removes the word if it was the last page.
        rest <- if nPages > page then go ws (w : ws') page imgs'
                                 else go ws ws'       page imgs'

        return $ res ++ rest

    -- Fetches the given list of images, ignores duplicates.
    goPix []       imgs = return ([], imgs)
    goPix (p : ps) imgs | S.member picId imgs = goPix ps imgs
                        | otherwise           = E.handle onException $ do
        sizes <- F.flick $ F.getSizes picId
        let Just small = find ((== "Small 320") . F.sizeDetailsLabel) sizes

        details <- F.flick $ F.getInfo picId (Just secret)

        let photoId  = T.pack picId
            title    = T.pack $ F.photoTitle p
            tags     = map (T.pack . F.tagDetailsName)
                          (F.photoDetailsTags details)
            urls     = F.photoDetailsURLs details
            url      = T.pack $ F.urlDetailsURL $ head urls
            imageUrl = F.sizeDetailsSource small
            ext      = takeExtension $ F.sizeDetailsSource small
            width    = F.sizeDetailsWidth  small
            height   = F.sizeDetailsHeight small
            photo    = Photo photoId title url tags imageUrl ext width height

        (rest, imgs') <- goPix ps (S.insert picId imgs)

        return (photo : rest, imgs')
      where
        picId  = F.photoId p
        secret = F.photoSecret p

        onException (_ :: E.SomeException) = goPix ps imgs
