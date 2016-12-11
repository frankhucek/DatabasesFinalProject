{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Web.Scotty as S
import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class
import Control.Monad
import Database.SQLite.Simple

db_file = "IADB.db"

-- my local port 1888
-- my public port 8080
-- frankhucek.com:8080

main :: IO ()
main = do
  putStrLn ("STARTING SCOTTY")
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
    artistInfo <- liftIO $ getArtistsByName search
    musicianInfo <- liftIO $ getMusiciansByName search
    genreInfo <- liftIO $ getAlbumsByGenre search
    serveQueryPage (formatStringList songInfo) (formatStringList albumInfo) (formatStringList artistInfo) (formatStringList musicianInfo) (formatStringList genreInfo)-- format Strings for print here



formatStringList :: [String] -> String
formatStringList info = foldr (++) [] $ fmap (++"<br/>") $ map replce info

serveStaticHTML :: String -> ActionM()
serveStaticHTML html_file = do
  html_raw <- liftIO $ readFile html_file
  html_header <- liftIO $ readFile "static/html/header.html"
  html_footer <- liftIO $ readFile "static/html/footer.html"
  S.html $ T.pack $ html_header ++ html_raw ++ html_footer


-- takes a song and puts it on a page
-- IN FUTURE TAKE TYPE => Songs -> Albums -> Artists -> etc for all tables
serveQueryPage :: String -> String -> String -> String -> String -> ActionM()
serveQueryPage songInfo albumInfo artistInfo musicianInfo genreInfo = do
  html_raw <- liftIO $ readFile "static/html/search.html"
  html_header <- liftIO $ readFile "static/html/header.html"
  html_footer <- liftIO $ readFile "static/html/footer.html"
  song_hdr <- liftIO $ readFile "static/html/songOut.html"
  album_hdr <- liftIO $ readFile "static/html/albumOut.html"
  artist_hdr <- liftIO $ readFile "static/html/artistOut.html"
  musician_hdr <- liftIO $ readFile "static/html/musicianOut.html"
  genre_hdr <- liftIO $ readFile "static/html/genreOut.html"
  S.html $ T.pack $ html_header ++ html_raw
    ++ "<br/><br/>" ++ song_hdr ++ songInfo
    ++ "<br/><br/>" ++ album_hdr ++ albumInfo
    ++ "<br/><br/>" ++ artist_hdr ++ artistInfo
    ++ "<br/><br/>" ++ musician_hdr ++ musicianInfo
    ++ "<br/><br/>" ++ genre_hdr ++ genreInfo
    ++ html_footer

replc :: Char -> Char
replc ',' = '\t'
replc x = x

replce :: String -> String
replce s = map replc s


{-
QUERIES
-}

-- print both return lists
getSongsByName :: String -> IO [String] -- name, length, genre; album name.
getSongsByName input = do
  db_conn <- open db_file -- Database connection, create 1 per connection if possible
  let input' = "%" ++ input ++ "%"
  songs <- (queryNamed db_conn "Select Name,Length,Genre from Songs where lower(Name) like :song" [":song" := input']) :: IO [(String, Int, String)]
  close db_conn
  return $ map show songs

getAlbumsByName :: String -> IO [String]
getAlbumsByName input = do
  db_conn <- open db_file -- Database connection, create 1 per connection if possible
  let input' = "%" ++ input ++ "%"
  albums <- (queryNamed db_conn "SELECT Name,Description FROM Albums WHERE lower(Name) like :album" [":album" := input']) :: IO [(String, String)]
  close db_conn
  return $ map show albums

getArtistsByName :: String -> IO [String]
getArtistsByName input = do
  db_conn <- open db_file
  let input' = "%" ++ input ++ "%"
  albums <- (queryNamed db_conn "SELECT Name,Description FROM Artists WHERE lower(Name) like :artist" [":artist" := input']) :: IO [(String,String)]
  close db_conn
  return $ map show albums

getMusiciansByName :: String -> IO [String]
getMusiciansByName input = do
  db_conn <- open db_file
  let input' = "%" ++ input ++ "%"
  albums <- (queryNamed db_conn "SELECT Musician_ID, Name FROM Musicians WHERE lower(Name) like :musician" [":musician" := input']) :: IO [(Int,String)]

  close db_conn
  return $ map show albums

getAlbumsByGenre :: String -> IO [String]
getAlbumsByGenre input = do
  db_conn <- open db_file
  let input' = "%" ++ input ++ "%"
  albums <- (queryNamed db_conn "select distinct Albums_and_Tracks.Name, Albums_and_Tracks.Description from (Albums inner join Tracks on Albums.Album_ID = Tracks.Album_ID) as Albums_and_Tracks inner join Songs on Songs.Song_ID = Albums_and_Tracks.Song_ID where lower(Songs.Genre) like :genre" [":genre" := input']) :: IO [(String,String)]
  close db_conn
  return $ map show albums

getTracksByAlbum :: String -> IO [String]
getTracksByAlbum input = do
  db_conn <- open db_file
  let input' = "%" ++ input ++ "%"
  albums <- (queryNamed db_conn "select Songs.Name, Songs.Length, Track_Number from (Albums inner join Tracks on Albums.Album_ID = Tracks.Album_ID) as Albums_and_Tracks inner join Songs on Songs.Song_ID = Albums_and_Tracks.Song_ID where lower(Albums.Name) like :album)" [":album" := input']) :: IO [(String,Int, Int)]
  close db_conn
  return $ map show albums
