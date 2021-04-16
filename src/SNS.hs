module SNS
  ( notify
  ) where

import Control.Lens            ((&), (?~))
import Control.Monad           (void)
import Control.Monad.Trans.AWS (send)
import Data.Text               (Text)
import Network.AWS             (MonadAWS, liftAWS)
import Network.AWS.SNS.Publish (pPhoneNumber, publish)

notify :: MonadAWS m => Text -> Text -> m ()
notify message phoneNumber = liftAWS $
  void $ send $ publish message & pPhoneNumber ?~ phoneNumber
