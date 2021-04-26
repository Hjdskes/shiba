module Main
  ( main
  ) where

import           Hedgehog.Main        (defaultMain)
import qualified SysTest.DynamoDBSpec as DynamoDBSpec

main :: IO ()
main = defaultMain
  [ DynamoDBSpec.tests
  ]
