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

  S.get "/songs" $ do
    songInfo <- liftIO $ getSongsByName ""
    serveDataPage "static/html/songOut.html" (formatStringList songInfo)

  S.get "/albums" $ do
    albumInfo <- liftIO $ getAlbumsByName ""
    serveDataPage "static/html/albumOut.html" (formatStringList albumInfo)

  S.get "/artists" $ do
    artistInfo <- liftIO $ getArtistsByName ""
    serveDataPage "static/html/artistOut.html" (formatStringList artistInfo)

  S.get "/musicians" $ do
    musicianInfo <- liftIO $ getMusiciansByName ""
    serveDataPage "static/html/musicianOut.html" (formatStringList musicianInfo)

  S.get "/searchquery" $ do
    -- get search query string
    search <- param "search_box" -- looks through query that called this route
    songInfo <- liftIO $ getSongsByName search
    albumInfo <- liftIO $ getAlbumsByName search
    artistInfo <- liftIO $ getArtistsByName search
    musicianInfo <- liftIO $ getMusiciansByName search
    genreInfo <- liftIO $ getAlbumsByGenre search
    trackInfo <- liftIO $ getTracksByAlbum search
    album_genre <- liftIO $ getGenreOfAlbum search
    serveQueryPage (formatStringList songInfo) (formatStringList albumInfo) (formatStringList artistInfo) (formatStringList musicianInfo) (formatStringList genreInfo) (formatStringList trackInfo) (formatStringList album_genre)  -- format Strings for print here

  S.get "/add" $ serveStaticHTML "static/html/add.html"

  S.get "/delete" $ serveStaticHTML "static/html/delete.html"

  S.get "/update" $ serveStaticHTML "static/html/update.html"

  -- INSERT PAGES
  S.post "/insertSong" $ do
    name <- param "songsName"
    len <- param "songsLength"
    lyr <- param "songsLyrics"
    rank <- param "songsRank"
    genre <- param "songsGenre"
    fp <- param "songsFilePath"
    liftIO $ insertSong name len lyr rank genre fp
    redirect "/add"

  S.post "/insertAlbum" $ do
    name <- param "albumsName"
    desc <- param "albumsDescription"
    release <- param "albumsReleaseDate"
    pic <- param "albumsPictureFilePath"
    sales_num <- param "albumsSalesNumber"
    label <- param "albumsRecordLabel"
    liftIO $ insertAlbum name desc release pic sales_num label
    redirect "/add"

  S.post "/insertArtist" $ do
    name <- param "artistsName"
    desc <- param "artistsDescription"
    form_date <- param "artistsFormationDate"
    pic <- param "artistsPictureFilePath"
    liftIO $ insertArtist name desc form_date pic
    redirect "/add"

  S.post "/insertMusician" $ do
    name <- param "musiciansName"
    dob <- param "musiciansDateOfBirth"
    liftIO $ insertMusician name dob
    redirect "/add"

  S.post "/insertTracks" $ do
    song_id <- param "tracksSongID"
    album_id <- param "tracksAlbumID"
    track_num <- param "tracksNumber"
    liftIO $ insertTrack song_id album_id track_num
    redirect "/add"

  S.post "/insertCreates" $ do
    sid <- param "createsSongID"
    aid <- param "createsArtistID"
    liftIO $ insertIntoCreates sid aid
    redirect "/add"

  S.post "/insertProduces" $ do
    albumid <- param "producesAlbumID"
    artistid <- param "producesArtistID"
    liftIO $ insertIntoProduces albumid artistid
    redirect "/add"

  S.post "/insertMemberOf" $ do
    mid <- param "memberOfMusicianID"
    aid <- param "memberOfArtistID"
    liftIO $ insertIntoMemberOf mid aid
    redirect "/add"

  -- DELETE PAGES
  S.post "/deleteSong" $ do
    sid <- param "songID"
    liftIO $ deleteSong sid
    redirect "/delete"

  S.post "/deleteAlbum" $ do
    aid <- param "albumID"
    liftIO $ deleteAlbum aid
    redirect "/delete"

  S.post "/deleteArtist" $ do
    aid <- param "artistID"
    liftIO $ deleteArtist aid
    redirect "/delete"

  S.post "/deleteMusician" $ do
    mid <- param "musicianID"
    liftIO $ deleteMusician mid
    redirect "/delete"

  S.post "/deleteProduces" $ do
    pid <- param "producesAlbumID"
    liftIO $ deleteProduces pid
    redirect "/delete"

  S.post "/deleteMemberOf" $ do
    mid <- param "memberOfMusicianID"
    liftIO $ deleteMemberOf mid
    redirect "/delete"

  S.post "/deleteCreates" $ do
    sid <- param "createsSongID"
    liftIO $ deleteCreates sid
    redirect "/delete"

  S.post "/deleteTracks" $ do
    sid <- param "tracksSongID"
    liftIO $ deleteTracks sid
    redirect "/delete"

  -- UPDATE PAGES
  S.post "/updateAlbum" $ do
    aid <- param "albumsID"
    name <- param "albumsName"
    desc <- param "albumsDescription"
    release <- param "albumsReleaseDate"
    pic <- param "albumsPictureFilePath"
    sales_num <- param "albumsSalesNumber"
    label <- param "albumsRecordLabel"
    liftIO $ updateAlbum aid name desc release pic sales_num label
    redirect "/update"


formatStringList :: [String] -> String
formatStringList info = foldr (++) [] $ fmap (++"<br/>") $ map replce info

serveStaticHTML :: String -> ActionM()
serveStaticHTML html_file = do
  html_raw <- liftIO $ readFile html_file
  html_header <- liftIO $ readFile "static/html/header.html"
  html_footer <- liftIO $ readFile "static/html/footer.html"
  S.html $ T.pack $ html_header ++ html_raw ++ html_footer

serveDataPage :: String -> String -> ActionM()
serveDataPage label_file attr_data = do
  html_label <- liftIO $ readFile label_file
  html_header <- liftIO $ readFile "static/html/header.html"
  html_footer <- liftIO $ readFile "static/html/footer.html"
  S.html $ T.pack $ html_header ++ html_label ++ attr_data ++ html_footer

-- takes a song and puts it on a page
-- IN FUTURE TAKE TYPE => Songs -> Albums -> Artists -> etc for all tables
serveQueryPage :: String -> String -> String -> String -> String -> String -> String -> ActionM()
serveQueryPage songInfo albumInfo artistInfo musicianInfo genreInfo trackInfo album_genre = do
  html_raw <- liftIO $ readFile "static/html/search.html"
  html_header <- liftIO $ readFile "static/html/header.html"
  html_footer <- liftIO $ readFile "static/html/footer.html"
  song_hdr <- liftIO $ readFile "static/html/songOut.html"
  album_hdr <- liftIO $ readFile "static/html/albumOut.html"
  artist_hdr <- liftIO $ readFile "static/html/artistOut.html"
  musician_hdr <- liftIO $ readFile "static/html/musicianOut.html"
  genre_hdr <- liftIO $ readFile "static/html/genreOut.html"
  track_hdr <- liftIO $ readFile "static/html/tracksOut.html"
  S.html $ T.pack $ html_header ++ html_raw
    ++ "<br/><br/>" ++ song_hdr ++ songInfo
    ++ "<br/><br/>" ++ album_hdr ++ albumInfo
    ++ "<br/><br/>" ++ artist_hdr ++ artistInfo
    ++ "<br/><br/>" ++ musician_hdr ++ musicianInfo
    ++ "<br/><br/>" ++ genre_hdr ++ genreInfo ++ "<br/><br/>" ++ album_genre
    ++ "<br/><br/>" ++ track_hdr ++ trackInfo
    ++ html_footer

replc :: Char -> Char
replc ',' = '\t'
replc x = x

replce :: String -> String
replce s = map replc s


{-
QUERIES
-}

-- SELECTS

-- print both return lists
getSongsByName :: String -> IO [String] -- name, length, genre; album name.
getSongsByName input = do
  db_conn <- open db_file -- Database connection, create 1 per connection if possible
  let input' = "%" ++ input ++ "%"
  songs <- (queryNamed db_conn "Select Song_ID,Name,Length,Genre from Songs where lower(Name) like :song" [":song" := input']) :: IO [(Int,String, Int, String)]
  close db_conn
  return $ map show songs

getAlbumsByName :: String -> IO [String]
getAlbumsByName input = do
  db_conn <- open db_file -- Database connection, create 1 per connection if possible
  let input' = "%" ++ input ++ "%"
  albums <- (queryNamed db_conn "SELECT Album_ID,Name,Description FROM Albums WHERE lower(Name) like :album" [":album" := input']) :: IO [(Int,String, String)]
  close db_conn
  return $ map show albums

getArtistsByName :: String -> IO [String]
getArtistsByName input = do
  db_conn <- open db_file
  let input' = "%" ++ input ++ "%"
  albums <- (queryNamed db_conn "SELECT Artist_ID,Name,Description FROM Artists WHERE lower(Name) like :artist" [":artist" := input']) :: IO [(Int,String,String)]
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
  albums <- (queryNamed db_conn "select Songs.Name, Songs.Length, Track_Number from (Albums inner join Tracks on Albums.Album_ID = Tracks.Album_ID) as Albums_and_Tracks inner join Songs on Songs.Song_ID = Albums_and_Tracks.Song_ID where lower(Albums.Name) like :album" [":album" := input']) :: IO [(String, Int, Int)]
  close db_conn
  return $ map show albums

getGenreOfAlbum :: String -> IO [String]
getGenreOfAlbum input = do
  db_conn <- open db_file
  let input' = "%" ++ input ++ "%"
  albums <- (queryNamed db_conn "select distinct Songs.Genre, Albums_and_Tracks.Name from (Albums inner join Tracks on Albums.Album_ID = Tracks.Album_ID) as Albums_and_Tracks inner join Songs on Songs.Song_ID = Albums_and_Tracks.Song_ID where lower(Albums.Name) like :genre" [":genre" := input']) :: IO [(String,String)]
  close db_conn
  return $ map show albums

getAlbumsByDateRange :: String -> String -> IO [String] -- no error checking on date values
getAlbumsByDateRange start end = do
  db_conn <- open db_file
  albums <- (queryNamed db_conn "Select distinct Albums.Name, Albums.Description, Albums.Release_Date From Albums Where (Albums.Release_Date < :end and Albums.Release_Date > :start)" [":start" := start, ":end" := end]) :: IO [(String, String, T.Text)]
  close db_conn
  return $ map show albums


-- INSERTS
-- NO BACKEND ERROR CHECKING IS DONE ON INSERTS; must fill in every text box

insertSong :: String -> Int -> String -> Int -> String -> String -> IO ()
insertSong name len lyrics rank genre file_path = do
  db_conn <- open db_file
  executeNamed db_conn "INSERT INTO Songs (Name, Length, Lyrics, Rank, Genre, File_Path) VALUES (:name, :len, :lyrics, :rank, :genre, :fp)" [":name" := name, ":len" := len, ":lyrics" := lyrics, ":rank" := rank, ":genre" := genre, ":fp" := file_path]
  close db_conn

insertAlbum :: String -> String -> String -> String -> Int -> String -> IO ()
insertAlbum name desc release pic sales_num label = do
  db_conn <- open db_file
  executeNamed db_conn "INSERT INTO Albums (Name, Description, Release_Date, Picture_File_Path, Sales_Number, Record_Label) VALUES (:name, :desc, :release, :pic, :sales_num, :label)" [":name" := name, ":desc" := desc, ":release" := release, ":pic" := pic, ":sales_num" := sales_num, ":label" := label]
  close db_conn

insertArtist :: String -> String -> String -> String  -> IO ()
insertArtist name desc form_date pic = do
  db_conn <- open db_file
  executeNamed db_conn "INSERT INTO Artists (Name, Description, Formation_Date, Picture_File_Path) VALUES (:name, :desc, :form_date, :pic)" [":name" := name, ":desc" := desc, ":form_date" := form_date, ":pic" := pic]
  close db_conn

insertMusician :: String -> String -> IO ()
insertMusician name dob = do
  db_conn <- open db_file
  executeNamed db_conn "INSERT INTO Musicians (Name, Date_Of_Birth) VALUES (:name, :dob)" [":name" := name, ":dob" := dob]
  close db_conn

insertTrack :: Int -> Int -> Int -> IO ()
insertTrack sid aid track = do
  db_conn <- open db_file
  executeNamed db_conn "INSERT INTO Tracks (Song_ID, Album_ID, Track_Number) VALUES (:sid, :aid, :track)" [":sid" := sid, ":aid" := aid, ":track" := track]
  close db_conn

insertIntoCreates :: Int -> Int -> IO ()
insertIntoCreates sid aid = do
  db_conn <- open db_file
  executeNamed db_conn "INSERT INTO Creates (Song_ID, Album_ID) VALUES (:sid, :aid)" [":sid" := sid, ":aid" := aid]
  close db_conn

insertIntoProduces :: Int -> Int -> IO ()
insertIntoProduces albumid artistid = do
  db_conn <- open db_file
  executeNamed db_conn "INSERT INTO Produces (Artist_ID, Album_ID) VALUES (:artistid, :albumid)" [":artistid" := artistid, ":albumid" := albumid]
  close db_conn

insertIntoMemberOf :: Int -> Int -> IO ()
insertIntoMemberOf mid aid = do
  db_conn <- open db_file
  executeNamed db_conn "INSERT INTO Member_Of (Artist_ID, Musician_ID) VALUES (:aid, :mid)" [":aid" := aid, ":mid" := mid]
  close db_conn

-- DELETES
deleteSong :: Int -> IO ()
deleteSong sid = do
  db_conn <- open db_file
  executeNamed db_conn "DELETE FROM Songs WHERE Song_ID = :sid" [":sid" := sid]
  close db_conn

deleteAlbum :: Int -> IO ()
deleteAlbum aid = do
  db_conn <- open db_file
  executeNamed db_conn "DELETE FROM Albums WHERE Album_ID = :aid" [":aid" := aid]
  close db_conn

deleteArtist :: Int -> IO ()
deleteArtist aid = do
  db_conn <- open db_file
  executeNamed db_conn "DELETE FROM Artists WHERE Artist_ID = :aid" [":aid" := aid]
  close db_conn

deleteMusician :: Int -> IO ()
deleteMusician mid = do
  db_conn <- open db_file
  executeNamed db_conn "DELETE FROM Musicians WHERE Musician_ID = :mid" [":mid" := mid]
  close db_conn

deleteProduces :: Int -> IO ()
deleteProduces aid = do
  db_conn <- open db_file
  executeNamed db_conn "DELETE FROM Produces WHERE Album_ID = :aid" [":aid" := aid]
  close db_conn

deleteMemberOf :: Int -> IO ()
deleteMemberOf mid = do
  db_conn <- open db_file
  executeNamed db_conn "DELETE FROM Member_Of WHERE Musician_ID = :mid" [":mid" := mid]
  close db_conn

deleteCreates :: Int -> IO ()
deleteCreates sid = do
  db_conn <- open db_file
  executeNamed db_conn "DELETE FROM Creates WHERE Song_ID = :sid" [":sid" := sid]
  close db_conn

deleteTracks :: Int -> IO ()
deleteTracks sid = do
  db_conn <- open db_file
  executeNamed db_conn "DELETE FROM Tracks WHERE Song_ID = :sid" [":sid" := sid]
  close db_conn

-- UPDATE QUERIES
updateAlbum :: Int -> String -> String -> String -> String -> Int -> String -> IO ()
updateAlbum aid name desc release pic sales_num label = do
  db_conn <- open db_file
  withTransaction db_conn $ executeNamed db_conn "UPDATE Albums set Name = :name, Description = :desc, Release_Date = :release, Picture_File_Path = :pic, Sales_Number = :sales_num, Record_Label = :label where Album_ID = :aid" [":aid" := aid, ":name" := name, ":desc" := desc, ":release" := release, ":pic" := pic, ":sales_num" := sales_num, ":label" := label]
  close db_conn
