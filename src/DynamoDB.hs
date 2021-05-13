module DynamoDB
  ( UpsertResult (..)
  , upsert
  ) where

import           Control.Exception.Lens       (handling)
import           Control.Lens                 ((&), (.~), (?~), (^.))
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.Trans.AWS      (AWST, ServiceError, send)
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.HashMap.Strict          as HashMap (fromList, null)
import           Data.Text                    (Text)
import           Network.AWS                  (MonadAWS, liftAWS)
import           Network.AWS.DynamoDB

-- | The result of the 'upsert' operation.
data UpsertResult a
  = ItemInserted a
  | ItemUnchanged a
  | ItemUpdated a
  deriving (Eq, Show)

tableName :: Text
tableName = "scraper_key_value_store"

-- | This function performs a conditional 'PutItem' request to DynamoDB.
--
-- Three things can happen when such a request is made:
--   1. The key is not found. In this case, the item is inserted without evaluation the condition.
--   2. The key is found. In this case, the condition is evaluated. That is, the stored value for the
--      `scraped` attribute is compared for (string-) equality to the `scraped` attribute in the new item.
--   3.1. If these are different, the new value is inserted for this attribute.
--   3.2. If they are equal, the old value remains and no write is made.
--
-- This works beautifully, if not for the fact that DynamoDB responds with a 400 Bad Request when the
-- conditional expression returns `false`. We work around this by catching a 400 response with that service
-- error only, and treat this case as the `ItemUnchanged` result.
update :: MonadCatch m => MonadResource m => Text -> Text -> AWST m (UpsertResult Text)
update key value = processResponse <$> send request
  where
    item = HashMap.fromList [ ("website", attributeValue & avS ?~ key), ("scraped", attributeValue & avS ?~ value) ]
    expressionAttributeValues = HashMap.fromList [ (":scraped", attributeValue & avS ?~ value) ]
    conditionExpression = "scraped <> :scraped"
    request = putItem tableName & piItem .~ item
      & piReturnValues ?~ AllOld
      & piExpressionAttributeValues .~ expressionAttributeValues
      & piConditionExpression ?~ conditionExpression
    processResponse :: PutItemResponse -> UpsertResult Text
    processResponse response = if HashMap.null (response ^. pirsAttributes)
      then ItemInserted value
      else ItemUpdated value

-- | Upsert (insert or update) a new (key, value) pair into the table.
--
-- The table and its layout are hardcoded in this function. The key is expected to be
-- the url of the scraped page, and the value the scraped result. To compare values for
-- the potential update, a simple string comparison is used.
upsert :: MonadAWS m => Text -> Text -> m (UpsertResult Text)
upsert key value = liftAWS $ handling _ConditionalCheckFailedException handleUnchanged $ update key value
  where
    handleUnchanged :: Applicative m => ServiceError -> m (UpsertResult Text)
    handleUnchanged _ = pure $ ItemUnchanged value
