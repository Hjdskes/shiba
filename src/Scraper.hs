module Scraper
  ( handler
  ) where

import Aws.Lambda
import Config                       (AppConfig (..))
import Control.Monad.Catch          (MonadCatch)
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Data.IORef                   (readIORef)
import Data.Text                    (Text)
import DynamoDB                     (UpsertResult (..), upsert)
import Notify                       (notify)
import Scrape                       (scrape)
import Text.HTML.Scalpel            (Scraper, hasClass, text, (@:))

-- | A grouping of a url to scrape and a scraper to execute on its page.
data ScrapeTarget str a = ScrapeTarget
  { url     :: Text
    -- ^ The url of the website to scrape.
  , scraper :: Scraper str a
    -- ^ The scraper to run on the retrieved page.
  }

scrapeTarget :: ScrapeTarget Text Text
scrapeTarget =
  ScrapeTarget
    { url = "https://blog.sutamuroku.com/besok/"
    , scraper = text $ "div" @: [hasClass "entry-content"]
    }

data ScrapeResult url new = NoChange url | TargetChanged url new

checkForChange :: MonadCatch m => MonadUnliftIO m => AppConfig -> ScrapeTarget Text Text -> m (Either String (ScrapeResult Text Text))
checkForChange appConfig ScrapeTarget{..} =
  -- TODO: deal with exceptions
  scrape url scraper >>= \case
    Just scraped -> do
      -- TODO: deal with exceptions
      res <- upsert appConfig url scraped
      return $ case res of
        ItemUpdated item  -> Right $ TargetChanged url item
        ItemInserted item -> Right $ TargetChanged url item
        ItemUnchanged _   -> Right $ NoChange url
    Nothing -> return $ Left "Failed to scrape"

sendSms :: MonadCatch m => MonadUnliftIO m => AppConfig -> ScrapeResult Text a -> m ()
sendSms _ (NoChange _) = pure ()
sendSms appConfig (TargetChanged url _) = mapM_ (notify appConfig message) phoneNumbers
  where message = url <> " has changed. Press the link to take a look!"
        phoneNumbers = [ "+46704350740", "+31624364852" ]

handler :: () -> Context AppConfig -> IO (Either String ())
handler _request context = do
  appConfig <- readIORef $ customContext context
  scrapeResult <- checkForChange appConfig scrapeTarget
  case scrapeResult of
    Left err  -> return $ Left err
    Right res -> do
      _ <- sendSms appConfig res
      return $ Right ()
