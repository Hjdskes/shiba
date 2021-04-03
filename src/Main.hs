{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Text.HTML.Scalpel
import Text.StringLike (StringLike)

besok :: Scraper String String
besok = text $ "div" @: [hasClass "entry-content"]

config :: StringLike str => Config str
config = Config utf8Decoder Nothing

main :: IO ()
main = do
  let url = "https://blog.sutamuroku.com/besok/"
  scrapeURLWithConfig config url besok >>= \case
    Just scraped -> do
      putStrLn $ "Scraped " <> url
    Nothing -> putStrLn $ "Failed to scrape " <> url