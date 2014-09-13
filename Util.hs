module Util (withHttpSqlite) where

import Control.Monad.Logger (LoggingT, runStderrLoggingT)
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistT, runSqlConn, withSqliteConn)
import Network.HTTP.Client (Manager, managerResponseTimeout, withManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

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
