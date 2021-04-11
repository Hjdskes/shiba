module Scraper
  ( handler
  ) where

import Aws.Lambda
import Config                       (AppConfig (..))
import Control.Monad.Catch          (MonadCatch)
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Data.IORef                   (readIORef)
import Data.Text                    (Text, pack)
import DynamoDB                     (PersistenceResult (..), persist)
import Scrape                       (scrape)
import Text.HTML.Scalpel            (Scraper, hasClass, text, (@:))

-- | A grouping of a url to scrape and a scraper to execute on its page.
data ScrapeTarget str a = ScrapeTarget
  { url     :: Text
    -- ^ The url of the website to scrape.
  , scraper :: Scraper str a
    -- ^ The scraper to run on the retrieved page.
  }

scrapeTarget :: ScrapeTarget String String
scrapeTarget =
  ScrapeTarget
    { url = "https://blog.sutamuroku.com/besok/"
    , scraper = text $ "div" @: [hasClass "entry-content"]
    }

-- TODO: deal with exceptions
checkForChange :: MonadCatch m => MonadUnliftIO m => AppConfig -> ScrapeTarget String String -> m (Either String ())
checkForChange appConfig ScrapeTarget{..} =
  fmap pack <$> scrape url scraper >>= \case
    Just scraped -> do
      res <- persist appConfig url scraped
      return $ case res of
        Failed status  -> Left $ "Received failure response from DynamoDB: " <> show status
        ItemInserted _ -> Right ()
        ItemUpdated _  -> Right ()
    Nothing -> return $ Left "Failed to scrape"

handler :: String -> Context AppConfig -> IO (Either String ())
handler _request context = do
  appConfig <- readIORef $ customContext context
  checkForChange appConfig scrapeTarget
