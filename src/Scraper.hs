module Scraper
  ( main
  ) where

import Scraper.Shiba

main :: IO ()
main = scrape >>= \case
  Just scraped ->
    putStrLn $ "Scraped " <> scraped
  Nothing ->
    putStrLn "Failed to scrape"
