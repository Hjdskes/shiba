module Notify
  ( notify
  ) where

import Config                       (AppConfig (..))
import Control.Lens                 ((&), (?~))
import Control.Monad                (void)
import Control.Monad.Catch          (MonadCatch)
import Control.Monad.Trans.AWS      (AWST', HasEnv, runAWST, runResourceT, send)
import Control.Monad.Trans.Resource (MonadUnliftIO, ResourceT)
import Data.Text                    (Text)
import Network.AWS.SNS.Publish      (pPhoneNumber, publish)

withSns :: HasEnv r => MonadUnliftIO m => r -> AWST' r (ResourceT m) a -> m a
withSns env = runResourceT . runAWST env

notify :: MonadCatch m => MonadUnliftIO m => AppConfig -> Text -> Text -> m ()
notify AppConfig{..} message phoneNumber = withSns env $
  void $ send $ publish message & pPhoneNumber ?~ phoneNumber
