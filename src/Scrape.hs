module Scrape
  ( scrape
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text              (Text, unpack)
import Text.HTML.Scalpel      hiding (scrape)
import Text.StringLike        (StringLike)

-- | Scrape the given url using the given scraper. A 'Just' is returned if the
-- scraping succeeded. On failure, 'Nothing' is returned.
--
-- This function uses the global manager provided by http-client-tls.
-- Any exceptions thrown by http-client are not caught and are bubbled up to the caller.
scrape :: MonadIO m => StringLike str => Text -> Scraper str a -> m (Maybe a)
scrape url = liftIO . scrapeURL (unpack url)
