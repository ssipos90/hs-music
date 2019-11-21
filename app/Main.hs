import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory)
import Lib
import           Protolude               hiding ( readFile )
import           Data.ByteString.Lazy.Char8     ( readFile )

main :: IO ()
main = do
  let fp = "playlists.yaml"
  content <- readFile fp
  case decodeYAML content of
    Left (p, error) -> TIO.putStrLn $ T.pack error
    Right (pls:_) -> do
      cwd <- getCurrentDirectory
      syncPlaylists pls cwd
