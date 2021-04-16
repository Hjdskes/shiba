module Scraper
  ( handler
  ) where

import Aws.Lambda
import Config                       (AppConfig (..))
import Control.Monad.Trans.AWS      (runAWST, runResourceT)
import Data.Aeson                   (Object)
import Data.IORef                   (readIORef)
import Data.Text                    (Text)
import DynamoDB                     (UpsertResult (..), upsert)
import Network.AWS                  (MonadAWS, liftAWS)
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

checkForChange :: MonadAWS m => ScrapeTarget Text Text -> m (Either String (ScrapeResult Text Text))
checkForChange ScrapeTarget{..} =
  -- TODO: deal with exceptions
  scrape url scraper >>= \case
    Just scraped -> do
      -- TODO: deal with exceptions
      res <- liftAWS $ upsert url scraped
      return $ case res of
        ItemUpdated item  -> Right $ TargetChanged url item
        ItemInserted item -> Right $ TargetChanged url item
        ItemUnchanged _   -> Right $ NoChange url
    Nothing -> return $ Left "Failed to scrape"

sendSms :: MonadAWS m => ScrapeResult Text a -> m ()
sendSms (NoChange _) = pure ()
sendSms (TargetChanged url _) = mapM_ (liftAWS . notify message) phoneNumbers
  where message = url <> " has changed. Press the link to take a look!"
        phoneNumbers = [ "+46704350740", "+31624364852" ]

handler :: Object -> Context AppConfig -> IO (Either String ())
handler _request context = do
  appConfig <- readIORef $ customContext context
  scrapeResult <- runResourceT . runAWST (env appConfig) $ checkForChange scrapeTarget
  case scrapeResult of
    Left err  -> return $ Left err
    Right res -> do
      _ <- runResourceT . runAWST (env appConfig) $ sendSms res
      return $ Right ()
