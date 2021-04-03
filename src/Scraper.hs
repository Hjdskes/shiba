module Scraper
  ( AppConfig(..)
  , initializeAppConfig
  , handler
  ) where

import Aws.Lambda
import Scraper.Shiba

data AppConfig =  AppConfig

initializeAppConfig :: IO AppConfig
initializeAppConfig = return AppConfig

handler :: String -> Context AppConfig -> IO (Either String ())
handler _request _context = scrape >>= \case
  Just scraped -> do
    putStrLn $ "Scraped " <> scraped
    return $ Right ()
  Nothing -> do
    putStrLn "Failed to scrape"
    return $ Left "Failed to scrape"