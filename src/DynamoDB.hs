module DynamoDB
  ( PersistenceResult (..)
  , persist
  ) where

import           Config                       (AppConfig (..))
import           Control.Lens                 ((&), (.~), (?~), (^.))
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.Trans.AWS      (AWST', HasEnv, runAWST,
                                               runResourceT, send)
import           Control.Monad.Trans.Resource (MonadUnliftIO, ResourceT)
import qualified Data.HashMap.Strict          as HashMap (fromList, lookup,
                                                          null)
import           Data.Maybe                   (fromJust)
import           Data.Text                    (Text)
import           Network.AWS.DynamoDB         (attributeValue, avS,
                                               piConditionExpression,
                                               piExpressionAttributeValues,
                                               piItem, piReturnValues,
                                               pirsAttributes,
                                               pirsResponseStatus, putItem)
import           Network.AWS.DynamoDB.Types   (ReturnValue (AllOld))

withDynamoDB :: HasEnv r => MonadUnliftIO m => r -> AWST' r (ResourceT m) a -> m a
withDynamoDB env action = runResourceT . runAWST env $ action

data PersistenceResult a = ItemInserted a | ItemUpdated a | Failed

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
            then ItemInserted value
            else ItemUpdated (fromJust $ HashMap.lookup "scraped" returnValues >>= \m -> m ^. avS)
        else Failed
