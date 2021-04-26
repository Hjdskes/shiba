module SysTest.DynamoDBSpec
  ( tests
  ) where

import           Control.Exception.Lens       (handling)
import           Control.Lens                 ((<&>), (&), (.~), (?~), (^.))
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Trans.AWS      (AWST, ServiceError, send)
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap (fromList, null)
import           Data.Text                    (Text)
import           Data.Text.Encoding           (encodeUtf8)
import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import           Hedgehog.Range               as Range
import           Network.AWS                  (AccessKey (..), Credentials (FromKeys), Env,
                                               MonadAWS, SecretKey (..), configure, liftAWS, newEnv,
                                               setEndpoint)
import           Network.AWS.DynamoDB

testAwsEnv :: MonadIO m => MonadCatch m => m Env
testAwsEnv = newEnv credentials <&> configure dynamoDB'
  where
    credentials = FromKeys (AccessKey "test") (SecretKey "test")
    dynamoDB' = setEndpoint False (encodeUtf8 "localhost") 4566 dynamoDB

tableName :: Text
tableName = "scraper_key_value_store"

genText :: Gen Text
genText = Gen.text (Range.linear 1 100) Gen.ascii -- TODO: utf8 or ascii or latin-1.

genAttributeValue :: Gen AttributeValue
genAttributeValue = undefined
-- genAttributeValue = genText <$> attributeValue & avS ?~

genScrapeResult :: (Text, Text)
genScrapeResult = undefined

-- TODO: test insert, update, change.
-- TODO: test error condition?

prop_newItemReturnsItemInserted :: Property
prop_newItemReturnsItemInserted = undefined

prop_differentItemReturnsItemUpdated :: Property
prop_differentItemReturnsItemUpdated = undefined

prop_unchangedItemReturnsItemUnchanged :: Property
prop_unchangedItemReturnsItemUnchanged = undefined

tests :: IO Bool
tests = do
  awsEnv <- testAwsEnv
  checkSequential $ Group "DynamoDBSpec"
    [ ("prop_newItemReturnsItemInserted", prop_newItemReturnsItemInserted)
    , ("prop_differentItemReturnsItemUpdated", prop_differentItemReturnsItemUpdated)
    , ("prop_unchangedItemReturnsItemUnchanged", prop_unchangedItemReturnsItemUnchanged)
    ]
      -- it "can insert a scrape result" . unit
      --   "Create a scrape result..." & annotate
      --   result <- forAll genScrapeResult
      --   "... and persist it in DynamoDB..." & annotate
