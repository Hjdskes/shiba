module Scraper.Shiba
  ( scrape
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text              (Text, unpack)
import Text.HTML.Scalpel      hiding (scrape)
import Text.StringLike        (StringLike)

config :: StringLike str => Config str
config = Config utf8Decoder Nothing

scrape :: MonadIO m => StringLike str => Text -> Scraper str a -> m (Maybe a)
scrape url = liftIO . scrapeURLWithConfig config (unpack url)
