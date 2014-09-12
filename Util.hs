module Util (withHttpSqlite) where

import Control.Lens
import Control.Monad.Logger (LoggingT, runStderrLoggingT)
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistT, runSqlConn, withSqliteConn)
import Network.HTTP.Client (managerResponseTimeout, withManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq (Options, defaults, manager)

-- | Runs the given action with an HTTP manager and a SQLite connection.
withHttpSqlite :: String -> (Options -> SqlPersistT (LoggingT IO) a) -> IO a
withHttpSqlite sqliteFile action =
    withManager settings $ \httpMan -> do
        let options = defaults & manager .~ Right httpMan

        runStderrLoggingT $
            withSqliteConn (T.pack sqliteFile) $ \conn ->
                runSqlConn (action options) conn
  where
    settings = tlsManagerSettings {
                      managerResponseTimeout = Just maxBound
                    }
