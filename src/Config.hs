module Config
  ( AppConfig (..)
  , initializeAppConfig
  ) where

import Control.Lens            (set, (<&>))
import Control.Monad.Trans.AWS (LogLevel (..), envLogger, newEnv, newLogger)
import Network.AWS             (Credentials (Discover), Env)
import System.IO               (stdout)

-- | A data type to hold the configuration of this application. As of now,
-- it is simply a newtype wrapper around Amazonka's 'Network.AWS.Env'.
newtype AppConfig
  = AppConfig { env :: Env }

-- | This function gets called from the AWS Lambda handler function and
-- sets up the 'AppConfig'. Its resulting value is shared between Lambda
-- invocations.
initializeAppConfig :: IO AppConfig
initializeAppConfig = do
  logger <- newLogger Info stdout
  AppConfig <$> (newEnv Discover <&> set envLogger logger)
