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
    albumInfo <- liftIO $ getAlbumsByName search
    serveQueryPage (formatStringList songInfo) (formatStringList albumInfo) -- format Strings for print here
    -- redirect "/search"



formatStringList :: [String] -> String
formatStringList info = foldr (++) [] $ fmap (++"<br/>") $ map replce info

serveStaticHTML :: String -> ActionM()
serveStaticHTML html_file = do
  html_raw <- liftIO $ readFile html_file
  html_header <- liftIO $ readFile "static/html/header.html"
  html_footer <- liftIO $ readFile "static/html/footer.html"
  S.html $ T.pack $ html_header ++ html_raw ++ html_footer

-- print both return lists
getSongsByName :: String -> IO [String] -- name, length, genre; album name.
getSongsByName input = do
  db_conn <- open "IADB.db" -- Database connection, create 1 per connection if possible
  let input' = "%" ++ input ++ "%"
  songs <- (queryNamed db_conn "Select Name,Length,Genre from Songs where lower(Name) like :song" [":song" := input']) :: IO [(String, Int, String)]
  close db_conn
  return $ map show songs

getAlbumsByName :: String -> IO [String]
getAlbumsByName input = do
  db_conn <- open "IADB.db" -- Database connection, create 1 per connection if possible
  let input' = "%" ++ input ++ "%"
  albums <- (queryNamed db_conn "SELECT Name,Description FROM Albums WHERE lower(Name) like :album" [":album" := input']) :: IO [(String, String)]
  close db_conn
  return $ map show albums

-- takes a song and puts it on a page
-- IN FUTURE TAKE TYPE => Songs -> Albums -> Artists -> etc for all tables
serveQueryPage :: String -> String -> ActionM()
serveQueryPage songInfo albumInfo = do
  html_raw <- liftIO $ readFile "static/html/search.html"
  html_header <- liftIO $ readFile "static/html/header.html"
  html_footer <- liftIO $ readFile "static/html/footer.html"
  S.html $ T.pack $ html_header ++ html_raw ++ songInfo ++ "<br/><br/>" ++ albumInfo ++ html_footer

replc :: Char -> Char
replc ',' = '\t'
replc x = x

replce :: String -> String
replce s = map replc s
