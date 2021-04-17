module Scrape
  ( scrape
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text              (Text, unpack)
import Text.HTML.Scalpel      hiding (scrape)
import Text.StringLike        (StringLike)

scrape :: MonadIO m => StringLike str => Text -> Scraper str a -> m (Maybe a)
scrape url = liftIO . scrapeURL (unpack url)
