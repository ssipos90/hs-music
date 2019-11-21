{-# LANGUAGE RecordWildCards #-}

import           Network.URL                    ( URL(URL)
                                                , URLType(Absolute)
                                                , Host(Host)
                                                , Protocol(HTTP)
                                                )
import           Test.Hspec
import           HsMusic
import           Protolude

makeUrl :: [Char] -> URL
makeUrl domain = URL (Absolute (Host (HTTP True) domain Nothing)) "" []

main :: IO ()
main = hspec $ describe "Decode YAML as expected" $ do
  it "Decodes and empty string to an empty array" $ do
    let expected = Right []
    decodeYAML "" `shouldBe` expected
  it "Decodes one item properly" $ do
    let playlistURL  = makeUrl "google.com"
    let playlistName = "minimal"
    let expected     = Right [[Playlist { .. }]]
    shouldBe (decodeYAML "- name: minimal\n  url: https://google.com") expected
