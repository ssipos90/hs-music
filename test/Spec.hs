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
main = hspec $
  describe "Decode YAML as expected" $ do
    it "decodes and empty string to an empty array" $ do
      let expected = Right []
      decodeYAML "" `shouldBe` expected
    it "fails if YAML does not respect schema" $
      -- we can safely ignore the error because it's YAML specific
      isLeft (decodeYAML "- name: minimal\n  some-field: some value") `shouldBe` True 
    it "decodes one item properly" $ do
      let playlistURL  = makeUrl "google.com"
      let playlistName = "minimal"
      let expected     = Right [[Playlist { .. }]]
      shouldBe (decodeYAML "- name: minimal\n  url: https://google.com") expected
