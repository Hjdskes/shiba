module DynamoDB
  ( UpsertResult (..)
  , upsert
  ) where

import           Config                       (AppConfig (..))
import           Control.Lens                 ((&), (.~), (?~), (^.))
import           Control.Monad                (void)
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.Trans.AWS      (AWST', HasEnv, runAWST,
                                               runResourceT, send)
import           Control.Monad.Trans.Resource (MonadUnliftIO, ResourceT)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap (fromList, insert,
                                                          lookup)
import           Data.Text                    (Text)
import           Network.AWS.DynamoDB         (attributeValue, avS, getItem,
                                               giKey, girsItem, piItem, putItem)
import           Network.AWS.DynamoDB.Types   (AttributeValue)

withDynamoDB :: HasEnv r => MonadUnliftIO m => r -> AWST' r (ResourceT m) a -> m a
withDynamoDB env = runResourceT . runAWST env

tableName :: Text
tableName = "scraper_key_value_store"

get :: MonadCatch m => MonadUnliftIO m => AppConfig -> HashMap Text AttributeValue -> m (Maybe Text)
get AppConfig{..} key = withDynamoDB env $ do
  result <- send $ getItem tableName & giKey .~ key
  return $ HashMap.lookup "scraped" (result ^. girsItem) >>= (^. avS)

set :: MonadCatch m => MonadUnliftIO m => AppConfig -> HashMap Text AttributeValue -> m ()
set AppConfig{..} item = withDynamoDB env $
  void $ send $ putItem tableName & piItem .~ item

data UpsertResult a = ItemInserted a | ItemUnchanged a | ItemUpdated a

upsert :: MonadCatch m => MonadUnliftIO m => AppConfig -> Text -> Text -> m (UpsertResult Text)
upsert config key value = do
  prev <- get config key'
  case prev of
    Just p | p == value ->
      return $ ItemUnchanged p
    Just _ -> do
      _ <- set config item
      return $ ItemUpdated value
    Nothing -> do
      _ <- set config item
      return $ ItemInserted value
  where
    key' = HashMap.fromList [ ("website", attributeValue & avS ?~ key) ]
    item = HashMap.insert "scraped" (attributeValue & avS ?~ value) key'
