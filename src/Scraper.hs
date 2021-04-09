module Scraper
  ( AppConfig(..)
  , initializeAppConfig
  , handler
  ) where

import           Aws.Lambda
import           Control.Lens                 (set, (&), (.~), (<&>), (?~),
                                               (^.))
import           Control.Monad.Trans.AWS      (AWST', HasEnv, LogLevel (..),
                                               envLogger, newEnv, newLogger,
                                               runAWST, runResourceT, send)
import           Control.Monad.Trans.Resource (MonadUnliftIO, ResourceT)
import qualified Data.HashMap.Strict          as HashMap (fromList, lookup)
import           Data.IORef                   (readIORef)
import           Data.Text                    (Text, pack, unpack)
import           Network.AWS                  (Credentials (Discover), Env)
import           Network.AWS.DynamoDB         (attributeValue, avS, getItem,
                                               giKey, girsItem, piItem, putItem)
import           Network.AWS.DynamoDB.PutItem (PutItemResponse)
import           Scraper.Shiba
import           System.IO                    (stdout)
import           Text.HTML.Scalpel            (Scraper, hasClass, text, (@:))

withDynamoDB :: HasEnv r => MonadUnliftIO m => r -> AWST' r (ResourceT m) a -> m a
withDynamoDB env action = runResourceT . runAWST env $ action

persist :: AppConfig -> Text -> Text -> IO PutItemResponse
persist AppConfig{..} key value = withDynamoDB env $
  send $ putItem tableName & piItem .~ item
  where item = HashMap.fromList
          [ ("website", attributeValue & avS ?~ key)
          , ("scraped", attributeValue & avS ?~ value)
          ]

retrieve :: AppConfig -> Text -> IO (Maybe Text)
retrieve AppConfig{..} key = withDynamoDB env $ do
  result <- send $ getItem tableName & giKey .~ key'
  return $ HashMap.lookup "scraped" (result ^. girsItem) >>= \m -> m ^. avS
  where key' = HashMap.fromList [ ("website", attributeValue & (avS ?~ key)) ]

data AppConfig = AppConfig
  { env       :: Env
  , tableName :: Text
  }

initializeAppConfig :: IO AppConfig
initializeAppConfig = do
  logger <- newLogger Debug stdout
  env <- newEnv Discover <&> set envLogger logger
  return $ AppConfig env "scraper_key_value_store"

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

checkForChange :: AppConfig -> ScrapeTarget String String -> IO (Either String ())
checkForChange appConfig ScrapeTarget{..} =
  fmap pack <$> scrape url scraper >>= \case
    Just scraped -> do
      putStrLn $ "Scraped " <> unpack scraped
      retrieve appConfig url >>= \case
        Nothing -> do
          putStrLn $ "No previous scrape result for " <> unpack url <> ", persisting new"
          _ <- persist appConfig url scraped
          return $ Right ()
        Just previous | previous == scraped -> do
          putStrLn $ "Previous scrape result for " <> unpack url <> " is identical to current, ignoring"
          return $ Right ()
        Just _ -> do
          putStrLn $ "Previous scrape result is different for " <> unpack url <> ", storing and notifying"
          _ <- persist appConfig url scraped
          return $ Right ()
    Nothing -> do
      putStrLn "Failed to scrape"
      return $ Left "Failed to scrape"

handler :: String -> Context AppConfig -> IO (Either String ())
handler _request context = do
  appConfig <- readIORef $ customContext context
  checkForChange appConfig scrapeTarget
