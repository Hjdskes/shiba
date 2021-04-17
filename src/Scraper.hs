module Scraper
  ( handler
  ) where

import           Aws.Lambda
import           Config                  (AppConfig (..))
import           Control.Exception.Lens  (handling)
import           Control.Monad.Trans.AWS (_Error, runAWST, runResourceT)
import           Data.Aeson              (Object)
import           Data.Either.Combinators (maybeToRight)
import           Data.IORef              (readIORef)
import           Data.Text               (Text)
import           DynamoDB                (UpsertResult (..), upsert)
import           Network.AWS             (Error, MonadAWS, liftAWS)
import qualified SNS                     (notify)
import           Scrape                  (scrape)
import           Text.HTML.Scalpel       (Scraper, hasClass, text, (@:))

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

check :: Traversable t => MonadAWS m => Text -> t Text -> m (t (ScrapeResult Text Text))
check url = mapM (\scraped -> toScrapeResult <$> liftAWS (upsert url scraped))
  where
    toScrapeResult (ItemInserted item) = TargetChanged url item
    toScrapeResult (ItemUpdated item)  = TargetChanged url item
    toScrapeResult (ItemUnchanged _)   = NoChange url

notify :: MonadAWS m => ScrapeResult Text a -> m ()
notify (NoChange _) = pure ()
notify (TargetChanged url _) = mapM_ (liftAWS . SNS.notify message) phoneNumbers
  where message = "🕵️ " <> url <> " has changed. Press the link to take a look! 🐾"
        phoneNumbers = [ "+46704350740", "+31624364852" ]

main :: MonadAWS m => ScrapeTarget Text Text -> m (Either String ())
main ScrapeTarget{..} = handling _Error errorToString $ do
  scraped <- maybeToRight ("Failed to scrape " <> show url) <$> scrape url scraper
  scrapeResult <- check url scraped
  mapM notify scrapeResult
  where
    errorToString :: Applicative m => Error -> m (Either String ())
    errorToString e = pure . Left $ "AWS exception scraping " <> show url <> ": " <> show e

handler :: Object -> Context AppConfig -> IO (Either String ())
handler _request context = do
  appConfig <- readIORef $ customContext context
  runResourceT . runAWST (env appConfig) $ main scrapeTarget
