import System.Environment (getArgs)

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
            return ()

        _            -> do
            putStrLn "usage: flickr-push-to-api <api root> <api key> \
                     \<sqlite db><dir> <tag>"
            putStrLn "where <api root> is the URL of end point of the API, \
                     \<api key> the private API key, <sqlite db> the database \
                     \which contains the image mapping, <dir> the directory \
                     \which contains the images and <tag> the tag which will \
                     \be used to group the images and to prefix their tags."

