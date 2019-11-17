{-# LANGUAGE RecordWildCards #-}

module Main where

import           Protolude               hiding ( readFile )
import           Control.Monad                  ( fail )
import           System.IO                      ( stdout, stderr )
import           System.Directory               ( getCurrentDirectory, createDirectoryIfMissing )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitWith
                                                , ExitCode(..)
                                                )
import           System.Process                 ( CreateProcess
                                                , createProcess
                                                , waitForProcess
                                                , proc
                                                , cwd
                                                , std_err
                                                , std_out
                                                , StdStream(..)
                                                )
import           Data.ByteString.Lazy.Char8     ( readFile
                                                , lines
                                                )
import           Data.Aeson                     ( eitherDecode
                                                , FromJSON
                                                , ToJSON
                                                , parseJSON
                                                , toJSON
                                                , withObject
                                                , withText
                                                , object
                                                , (.:)
                                                , (.=)
                                                )
import           Data.Either                    ( partitionEithers )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Network.URL                    ( URL
                                                , importURL
                                                , exportURL
                                                )

data Playlist = Playlist { playlistName :: Text
                         , playlistURL :: URL
                         } deriving (Show)

instance FromJSON URL where
  parseJSON = withText "String" $ \s -> case importURL $ T.unpack s of
    Just url -> return url
    Nothing  -> fail "invalid url"

instance FromJSON Playlist where
  parseJSON = withObject "playlist" $ \o -> do
    playlistName <- o .: "name"
    playlistURL  <- o .: "url"
    return Playlist { .. }

instance ToJSON Playlist where
  toJSON Playlist {..} =
    object ["name" .= playlistName, "url" .= exportURL playlistURL]

main :: IO ()
main = do
  let fp = "playlists.txt"
  content <- readFile fp
  let (errors, pls) = partitionEithers $ map
        (\line -> eitherDecode line :: Either [Char] Playlist)
        (lines content)
  TIO.putStrLn $ T.unlines $ map T.pack errors
  cwd <- getCurrentDirectory
  syncPlaylists pls cwd

syncPlaylists :: [Playlist] -> FilePath -> IO ()
syncPlaylists pls dir = do
  createDirectoryIfMissing False dir
  mapM_ (execCommand dir) pls

execCommand :: FilePath -> Playlist -> IO ExitCode
execCommand dir Playlist{..} = do
  TIO.putStrLn (playlistName <> " [" <> T.pack (exportURL playlistURL) <> "]" :: Text)
  let plDir = dir ++ "/" ++ T.unpack playlistName
  createDirectoryIfMissing False plDir
  (_,_,_, procHandle) <- createProcess $ createCommand plDir playlistURL
  waitForProcess procHandle

createCommand :: FilePath -> URL -> CreateProcess
createCommand dir playlistURL =
  let args =
          [ "--download-archive"
          , ".archive"
          , "-f"
          , "best"
          , T.pack $ exportURL playlistURL
          ]
  in  (proc "youtube-dl" (map T.unpack args))
                               { std_out = UseHandle stdout
                               , std_err = UseHandle stderr
                               , cwd     = Just dir
                               }


