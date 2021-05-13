module SysTest.DynamoDBSpec
  ( tests
  ) where

import           Control.Exception.Lens       (handling)
import           Control.Lens                 ((&), (<&>))
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Trans.AWS      (AWST, ServiceError, send)
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Text                    (Text)
import           Data.Text.Encoding           (encodeUtf8)
import           DynamoDB                     (UpsertResult (..), upsert)
import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import           Hedgehog.Range               as Range
import           Network.AWS                  (AccessKey (..), Credentials (FromKeys), Env,
                                               MonadAWS, SecretKey (..), configure, liftAWS, newEnv,
                                               setEndpoint)
import           Network.AWS.DynamoDB         (dynamoDB)

testAwsEnv :: MonadIO m => MonadCatch m => m Env
testAwsEnv = newEnv credentials <&> configure dynamoDB'
  where
    credentials = FromKeys (AccessKey "test") (SecretKey "test")
    dynamoDB' = setEndpoint False (encodeUtf8 "localhost") 4566 dynamoDB

genText :: Gen Text
genText = Gen.text (Range.linear 1 100) Gen.ascii -- TODO: utf8 or ascii or latin-1.

genScrapeResult :: Gen (Text, Text)
genScrapeResult = (,)
  <$> genText
  <*> genText

-- TODO: test insert, update, change.
-- TODO: test error condition?

prop_newItemReturnsItemInserted :: Property
prop_newItemReturnsItemInserted = property $ do
  "Create a scrape result..." & annotate
  (url, scraped) <- forAll genScrapeResult
  "... persist it in DynamoDB..." & annotate
  result <- liftAWS (upsert url scraped)
  "... the result should be ItemInserted of that result" & annotate
  result === ItemInserted scraped

-- prop_differentItemReturnsItemUpdated :: Property
-- prop_differentItemReturnsItemUpdated = undefined

-- prop_unchangedItemReturnsItemUnchanged :: Property
-- prop_unchangedItemReturnsItemUnchanged = undefined

tests :: IO Bool
tests = do
  awsEnv <- testAwsEnv
  checkSequential $ Group "DynamoDBSpec"
    [ ("prop_newItemReturnsItemInserted", prop_newItemReturnsItemInserted)
    -- , ("prop_differentItemReturnsItemUpdated", prop_differentItemReturnsItemUpdated)
    -- , ("prop_unchangedItemReturnsItemUnchanged", prop_unchangedItemReturnsItemUnchanged)
    ]
