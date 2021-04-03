module Scraper.Shiba
  ( scrape
  ) where

import Text.HTML.Scalpel hiding (scrape)
import Text.StringLike (StringLike)

besok :: Scraper String String
besok = text $ "div" @: [hasClass "entry-content"]

config :: StringLike str => Config str
config = Config utf8Decoder Nothing

scrape :: IO (Maybe String)
scrape = scrapeURLWithConfig config url besok
  where url = "https://blog.sutamuroku.com/besok/"