module Config
  ( AppConfig(..)
  , initializeAppConfig
  ) where

import Control.Lens            (set, (<&>))
import Control.Monad.Trans.AWS (LogLevel (..), envLogger, newEnv, newLogger)
import Network.AWS             (Credentials (Discover), Env)
import System.IO               (stdout)

newtype AppConfig = AppConfig { env :: Env }

initializeAppConfig :: IO AppConfig
initializeAppConfig = do
  logger <- newLogger Debug stdout
  AppConfig <$> (newEnv Discover <&> set envLogger logger)
