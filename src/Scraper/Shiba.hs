module Scraper.Shiba
  ( scrape
  ) where

import Data.Text         (Text, unpack)
import Text.HTML.Scalpel hiding (scrape)
import Text.StringLike   (StringLike)

config :: StringLike str => Config str
config = Config utf8Decoder Nothing

scrape :: StringLike str => Text -> Scraper str a -> IO (Maybe a)
scrape url = scrapeURLWithConfig config (unpack url)
