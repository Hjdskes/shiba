module DynamoDB
  ( UpsertResult (..)
  , upsert
  ) where

import           Control.Lens                 ((&), (.~), (?~), (^.))
import           Control.Monad                (void)
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.Trans.AWS      (AWST, send)
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap (fromList, insert,
                                                          lookup)
import           Data.Text                    (Text)
import           Network.AWS                  (MonadAWS, liftAWS)
import           Network.AWS.DynamoDB         (attributeValue, avS, getItem,
                                               giKey, girsItem, piItem, putItem)
import           Network.AWS.DynamoDB.Types   (AttributeValue)

data UpsertResult a = ItemInserted a | ItemUnchanged a | ItemUpdated a

tableName :: Text
tableName = "scraper_key_value_store"

get :: MonadCatch m => MonadResource m => HashMap Text AttributeValue -> AWST m (Maybe Text)
get key = do
  result <- send $ getItem tableName & giKey .~ key
  return $ HashMap.lookup "scraped" (result ^. girsItem) >>= (^. avS)

set :: MonadCatch m => MonadResource m => HashMap Text AttributeValue -> AWST m ()
set item = void $ send $ putItem tableName & piItem .~ item

upsert :: MonadAWS m => Text -> Text -> m (UpsertResult Text)
upsert key value = do
  prev <- liftAWS (get key')
  case prev of
    Just p | p == value ->
      return $ ItemUnchanged p
    Just _ -> do
      _ <- liftAWS (set item)
      return $ ItemUpdated value
    Nothing -> do
      _ <- liftAWS (set item)
      return $ ItemInserted value
  where
    key' = HashMap.fromList [ ("website", attributeValue & avS ?~ key) ]
    item = HashMap.insert "scraped" (attributeValue & avS ?~ value) key'
