module Config
  ( AppConfig(..)
  , initializeAppConfig
  ) where

import Control.Lens            (set, (<&>))
import Control.Monad.Trans.AWS (LogLevel (..), envLogger, newEnv, newLogger)
import Data.Text               (Text)
import Network.AWS             (Credentials (Discover), Env)
import System.IO               (stdout)

data AppConfig = AppConfig
  { env       :: Env
  , tableName :: Text
  }

initializeAppConfig :: IO AppConfig
initializeAppConfig = do
  logger <- newLogger Debug stdout
  env <- newEnv Discover <&> set envLogger logger
  return $ AppConfig env "scraper_key_value_store"
