{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Web.Scotty as S
import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class
import Control.Monad
import Database.SQLite.Simple


-- my local port 1888
-- my public port 8080
-- frankhucek.com:8080

main :: IO ()
main = do
  scotty 1888 $ do
    middleware $ staticPolicy $ hasPrefix "static/"
    routes


routes :: ScottyM ()
routes = do
  S.get "/" $ serveStaticHTML "static/html/index.html"
  S.get "/about" $ serveStaticHTML "static/html/about.html"
  S.get "/search" $ serveStaticHTML "static/html/search.html"
  S.get "/searchquery" $ do
    -- get search query string
    search <- param "search_box" -- looks through query that called this route
    songInfo <- liftIO $ getSongsByName search
    liftIO $ putStrLn $ show songInfo
    redirect "/search"


serveStaticHTML :: String -> ActionM()
serveStaticHTML html_file = do
  html_raw <- liftIO $ readFile html_file
  html_header <- liftIO $ readFile "static/html/header.html"
  html_footer <- liftIO $ readFile "static/html/footer.html"
  S.html $ T.pack $ html_header ++ html_raw ++ html_footer

getSongsByName :: String -> IO [(String, Int, String)] -- name, length, genre
getSongsByName song_name = do
  db_conn <- open "IADB.db" -- Database connection, create 1 per connection if possible
  -- let q = ("Select Name,Length,Genre from Songs where lower(Name) like '%" `mappend` song_name `mappend` "%'")
  let song_name' = "%" ++ song_name ++ "%"
  putStrLn $ song_name
  table <- queryNamed db_conn "Select Name,Length,Genre from Songs where lower(Name) like :song" [":song" := song_name']   -- implement later (Only (song_name :: String))
  close db_conn
  return table
