module Scraper
  ( handler
  ) where

import           Aws.Lambda
import           Config                  (AppConfig (..))
import           Control.Exception.Lens  (handling)
import           Control.Monad.Trans.AWS (_Error, runAWST, runResourceT)
import           Data.Aeson              (Object)
import           Data.Either             (lefts, rights)
import           Data.Either.Combinators (maybeToRight)
import           Data.IORef              (readIORef)
import           Data.Text               (Text, unpack)
import           DynamoDB                (UpsertResult (..), upsert)
import           Network.AWS             (Error, MonadAWS, liftAWS)
import qualified SNS                     (notify)
import           Scrape                  (scrape)
import           Text.HTML.Scalpel       (Scraper, hasClass, text, (@:), (@=))

-- | A grouping of a url to scrape and a 'Text.HTML.Scalpel.Scraper' to execute on its page.
data ScrapeTarget str a = ScrapeTarget
  { url     :: Text -- ^ The url of the website to scrape.
  , scraper :: Scraper str a -- ^ The 'Text.HTML.Scalpel.Scraper' to run on the retrieved page.
  }

-- | The result of scraping the page on the given url.
data ScrapeResult url new
  = NoChange url -- ^ The page on the given url has not changed.
  | TargetChanged url new -- ^ The page on the given url has changed; its new contents are returned.

-- | The list of pages to scrape.
scrapeTargets :: [ScrapeTarget Text Text]
scrapeTargets =
  [ ScrapeTarget
      { url = "https://blog.sutamuroku.com/besok/"
      , scraper = text $ "div" @: [ hasClass "entry-content" ]
      }
  , ScrapeTarget
      { url = "http://www.caccia.se/SHIBA_Puppies.htm"
      , scraper = text $ "table" @: [ "id" @= "table3" ]
      }
  , ScrapeTarget
      { url = "http://www.kennelposh.se/empty_9.html"
      , scraper = text $ "div" @: [ hasClass "ParagraphContainer" ]
      }
  ]

-- | Given a url and the result of scraping the page on that url, upserts the
-- scraping result into the database. The result of the upsert is transformed
-- into a 'ScrapeResult'.
check :: Traversable t => MonadAWS m => Text -> t Text -> m (t (ScrapeResult Text Text))
check url = mapM (\scraped -> toScrapeResult <$> liftAWS (upsert url scraped))
  where
    toScrapeResult (ItemInserted item) = TargetChanged url item
    toScrapeResult (ItemUpdated item)  = TargetChanged url item
    toScrapeResult (ItemUnchanged _)   = NoChange url

-- | Given a 'ScrapeResult', either sends an SMS message or not.
notify :: MonadAWS m => ScrapeResult Text a -> m ()
notify (NoChange _) = pure ()
notify (TargetChanged url _) = mapM_ (liftAWS . SNS.notify message) phoneNumbers
  where message = "🕵️ " <> url <> " has changed. Press the link to take a look! 🐾"
        phoneNumbers = [ "+46704350740", "+31624364852" ]

-- | The main business logic. This function composes the scraping, the upsert and the notifying for
-- a single 'ScrapeTarget'.
main :: MonadAWS m => ScrapeTarget Text Text -> m (Either String String)
main ScrapeTarget{..} = handling _Error errorToString $ do
  scraped <- maybeToRight ("Failed to scrape " <> unpack url) <$> scrape url scraper
  scrapeResult <- check url scraped
  mapM_ notify scrapeResult
  return $ Right ("Scraped " <> unpack url)
  where
    errorToString :: Applicative m => Error -> m (Either String String)
    errorToString e = pure . Left $ "AWS exception scraping " <> unpack url <> ": " <> show e

-- | The AWS Lambda handler. This is the entrypoint into our logic from AWS Lambda.
-- This function initialises the shared context (shared between Lambda invocations) and
-- invokes the 'main' function on each element in 'scrapeTargets'. The results are gathered;
-- if there are one or more 'Left' returned their error messages are concatenated and returned
-- as the Lambda's exit result. Only if there are no 'Left' is the Lambda invocation considered
-- successful. In this case, the list of scraped URLs is returned.
--
-- Since the Lambda is invoked on a schedule through CloudWatch, the input is ignored.
handler :: Object -> Context AppConfig -> IO (Either String String)
handler _request context = do
  appConfig <- readIORef $ customContext context
  gather <$> mapM (runResourceT . runAWST (env appConfig) . main) scrapeTargets
  where
    gather :: [Either String String] -> Either String String
    gather xs | null (lefts xs) = Right $ unlines (rights xs)
              | otherwise       = Left $ unlines (lefts xs)
