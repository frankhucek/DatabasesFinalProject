{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Web.Scotty as S
import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class
import Control.Monad

-- my local port 1888
-- my public port 8080
-- frankhucek.com:8080

main :: IO ()
main = scotty 1888 $ do
  middleware $ staticPolicy $ hasPrefix "static/"
  routes


routes :: ScottyM ()
routes = do
  S.get "/" $ serveStaticHTML "static/html/index.html"


serveStaticHTML :: String -> ActionM()
serveStaticHTML html_file = do
  html_raw <- liftIO $ readFile html_file
  liftIO $ putStrLn $ html_raw
  S.html $ T.pack html_raw
