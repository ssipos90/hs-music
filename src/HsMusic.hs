{-# LANGUAGE RecordWildCards #-}

module HsMusic
  ( Playlist(..)
  , decodeYAML
  , syncPlaylists
  )
where

import           Protolude               hiding ( readFile )
import           Control.Monad                  ( fail )
import           System.IO                      ( stdout
                                                , stderr
                                                )
import           System.Directory               ( createDirectoryIfMissing
                                                )
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
import qualified Data.ByteString.Lazy          as BSL
import           Data.YAML                      ( Pos
                                                , FromYAML
                                                , ToYAML
                                                , parseYAML
                                                , toYAML
                                                , withMap
                                                , withStr
                                                , decode
                                                , mapping
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
                         } deriving (Show, Eq)

instance FromYAML URL where
  parseYAML = withStr "String" $ \s -> case importURL $ T.unpack s of
    Just url -> return url
    Nothing  -> fail "invalid url"

instance FromYAML Playlist where
  parseYAML = withMap "Playlist" $ \m -> do
    playlistName <- m .: "name"
    playlistURL  <- m .: "url"
    return Playlist { .. }

instance ToYAML Playlist where
  toYAML Playlist {..} =
    mapping ["name" .= playlistName, "url" .= T.pack (exportURL playlistURL)]

decodeYAML :: BSL.ByteString -> Either (Pos, [Char]) [[Playlist]]
decodeYAML = decode

syncPlaylists :: [Playlist] -> FilePath -> IO ()
syncPlaylists pls dir = do
  createDirectoryIfMissing False dir
  mapM_ (execCommand dir) pls

execCommand :: FilePath -> Playlist -> IO ExitCode
execCommand dir Playlist {..} = do
  TIO.putStrLn
    (playlistName <> " [" <> T.pack (exportURL playlistURL) <> "]" :: Text)
  let plDir = dir ++ "/" ++ T.unpack playlistName
  createDirectoryIfMissing False plDir
  (_, _, _, procHandle) <- createProcess $ createCommand plDir playlistURL
  waitForProcess procHandle

createCommand :: FilePath -> URL -> CreateProcess
createCommand dir playlistURL =
  let args =
          [ "--download-archive"
          , ".archive"
          , "-f"
          , "best"
          , "-x"
          , T.pack $ exportURL playlistURL
          ]
  in  (proc "youtube-dl" (map T.unpack args)) { std_out = UseHandle stdout
                                              , std_err = UseHandle stderr
                                              , cwd     = Just dir
                                              }
