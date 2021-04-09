module Scraper
  ( AppConfig(..)
  , initializeAppConfig
  , handler
  ) where

import           Aws.Lambda
import           Control.Lens                 (set, (&), (.~), (<&>), (^.), (?~))
import           Control.Monad.Trans.AWS      (AWST', HasEnv, LogLevel (..),
                                               envLogger, newEnv, newLogger,
                                               reconfigure, runAWST,
                                               runResourceT, send, within)
import           Control.Monad.Trans.Resource (MonadUnliftIO, ResourceT)
import qualified Data.HashMap.Strict          as HashMap (fromList, lookup)
import           Data.IORef                   (readIORef)
import           Data.Text                    (Text)
import           Network.AWS                  (Credentials (Discover), Env,
                                               Region (..), Service)
import           Network.AWS.DynamoDB         (attributeValue, avS, dynamoDB,
                                               getItem, giKey, girsItem, piItem,
                                               putItem)
import           Network.AWS.DynamoDB.PutItem (PutItemResponse)
import           Scraper.Shiba
import           System.IO                    (stdout)
import           Text.HTML.Scalpel            (Scraper, hasClass, text, (@:))

withDynamoDB :: HasEnv r => MonadUnliftIO m => r -> Service -> Region -> AWST' r (ResourceT m) a -> m a
withDynamoDB env service region action =
  runResourceT . runAWST env . within region $ reconfigure service action

persist :: AppConfig -> Text -> Text -> IO PutItemResponse
persist AppConfig{..} key value = withDynamoDB env service region $
  send $ putItem tableName & piItem .~ item
  where item = HashMap.fromList
          [ ("website", attributeValue & avS ?~ key)
          , ("scraped", attributeValue & avS ?~ value)
          ]

retrieve :: AppConfig -> Text -> IO (Maybe Text)
retrieve AppConfig{..} key = withDynamoDB env service region $ do
  result <- send $ getItem tableName & giKey .~ key'
  return $ HashMap.lookup "scraped" (result ^. girsItem) >>= \m -> m ^. avS
  where key' = HashMap.fromList [ ("website", attributeValue & (avS ?~ key)) ]

data AppConfig = AppConfig
  { env       :: Env
  , service   :: Service
  , region    :: Region
  , tableName :: Text
  }

initializeAppConfig :: IO AppConfig
initializeAppConfig = do
  logger <- newLogger Debug stdout
  env <- newEnv Discover <&> set envLogger logger
  return $ AppConfig env dynamoDB Ireland "scraper_key_value_store"

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

handler :: String -> Context AppConfig -> IO (Either String ())
handler _request context = do
  _appConfig <- readIORef $ customContext context
  scrape (url scrapeTarget) (scraper scrapeTarget)>>= \case
    Just scraped -> do
      putStrLn $ "Scraped " <> scraped
      return $ Right ()
    Nothing -> do
      putStrLn "Failed to scrape"
      return $ Left "Failed to scrape"
