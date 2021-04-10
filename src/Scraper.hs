module Scraper
  ( AppConfig(..)
  , initializeAppConfig
  , handler
  ) where

import           Aws.Lambda
import           Control.Lens                 (set, (&), (.~), (<&>), (?~),
                                               (^.))
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.Trans.AWS      (AWST', HasEnv, LogLevel (..),
                                               envLogger, newEnv, newLogger,
                                               runAWST, runResourceT, send)
import           Control.Monad.Trans.Resource (MonadUnliftIO, ResourceT)
import qualified Data.HashMap.Strict          as HashMap (fromList, lookup,
                                                          null)
import           Data.IORef                   (readIORef)
import           Data.Maybe                   (fromJust)
import           Data.Text                    (Text, pack)
import           Network.AWS                  (Credentials (Discover), Env)
import           Network.AWS.DynamoDB         (attributeValue, avS,
                                               piConditionExpression,
                                               piExpressionAttributeValues,
                                               piItem, piReturnValues,
                                               pirsAttributes,
                                               pirsResponseStatus, putItem)
import           Network.AWS.DynamoDB.Types   (ReturnValue (AllOld))
import           Scraper.Shiba
import           System.IO                    (stdout)
import           Text.HTML.Scalpel            (Scraper, hasClass, text, (@:))

withDynamoDB :: HasEnv r => MonadUnliftIO m => r -> AWST' r (ResourceT m) a -> m a
withDynamoDB env action = runResourceT . runAWST env $ action

data PersistenceResult a = ItemInserted | ItemUpdated a | Failed

-- TODO: typeclass to make item?
persist :: MonadCatch m => MonadUnliftIO m => AppConfig -> Text -> Text -> m (PersistenceResult Text)
persist AppConfig{..} key value = withDynamoDB env $ processResponse <$> send request
  where
    item = HashMap.fromList [ ("website", attributeValue & avS ?~ key), ("scraped", attributeValue & avS ?~ value) ]
    expressionAttributeValues = HashMap.fromList [ (":scraped", attributeValue & avS ?~ value) ]
    conditionExpression = "scraped <> :scraped"
    request = putItem tableName & piItem .~ item
      & piReturnValues ?~ AllOld
      & piExpressionAttributeValues .~ expressionAttributeValues
      & piConditionExpression ?~ conditionExpression
    processResponse response =
      if (response ^. pirsResponseStatus) == 200
        then
          let returnValues = response ^. pirsAttributes
          in if (HashMap.null returnValues)
            then ItemInserted
            else ItemUpdated (fromJust $ HashMap.lookup "scraped" returnValues >>= \m -> m ^. avS)
        else Failed

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

-- TODO: deal with exceptions
checkForChange :: MonadCatch m => MonadUnliftIO m => AppConfig -> ScrapeTarget String String -> m (Either String ())
checkForChange appConfig ScrapeTarget{..} =
  fmap pack <$> scrape url scraper >>= \case
    Just scraped -> do
      res <- persist appConfig url scraped
      case res of
        Failed        -> return $ Left "Failed to insert or update DynamoDB"
        ItemInserted  -> return $ Right ()
        ItemUpdated _ -> return $ Right ()
    Nothing -> return $ Left "Failed to scrape"

handler :: String -> Context AppConfig -> IO (Either String ())
handler _request context = do
  appConfig <- readIORef $ customContext context
  checkForChange appConfig scrapeTarget
