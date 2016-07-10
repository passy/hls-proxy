{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

import           Control.Lens                 (to)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import qualified Data.Either.Combinators      as Either
import           Data.List.NonEmpty           (NonEmpty ((:|)))
import qualified Data.Set                     as Set
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as TIO
import qualified Lib.HLS.Parse                as HLS
import qualified Network.URI                  as URI
import           System.Directory             (getCurrentDirectory)
import           System.FilePath              ((</>))
import           Test.Hspec
import           Test.Hspec.Expectations.Lens (shouldView, through)
import qualified Text.Megaparsec              as M
import qualified Text.Megaparsec.Pos          as MP

openFixture :: forall a. (FilePath -> IO a) -> FilePath -> IO a
openFixture f path = do
    dir <- getCurrentDirectory
    f $ dir </> "test" </> "fixtures" </> path

openTextFixture :: FilePath -> IO T.Text
openTextFixture = openFixture TIO.readFile

main :: IO ()
main = hspec .
  describe "HLS Parser" $ do
    it "read master playlist version" $ do
      masterPlaylist <- openFixturePlaylist "master-playlist.m3u8"

      masterPlaylist `shouldSatisfy` Either.isRight
      unsafeFromRight masterPlaylist `shouldView` HLS.HLSVersion 3 `through` HLS.hlsVersion

    it "read master playlist with spaces" $ do
      masterPlaylist <- openFixturePlaylist "master-playlist-spaces.m3u8"
      masterPlaylist `shouldSatisfy` Either.isRight

    it "rejects invalid versioned master playlists" $ do
      masterPlaylist <- openFixturePlaylist "master-playlist-invalid-version.m3u8"
      masterPlaylist `shouldSatisfy` Either.isLeft
      let (Left e) = masterPlaylist
      let pos = MP.SourcePos { MP.sourceName = "", MP.sourceLine = MP.unsafePos 2, MP.sourceColumn = MP.unsafePos 18}
      e `shouldBe` M.ParseError { M.errorPos = pos :| []
                                , M.errorUnexpected = Set.empty
                                , M.errorExpected = Set.empty
                                , M.errorCustom = Set.singleton $ M.DecFail "Unsupported version 88" }

    it "extracts media playlist URIs" $ do
      masterPlaylist <- openFixturePlaylist "master-playlist.m3u8"
      let (Right res) = masterPlaylist
      res `shouldView` 2 `through` HLS.hlsEntries . to length

    it "recognizes master playlists" $ do
      masterPlaylist <- openFixturePlaylist "master-playlist.m3u8"
      let (Right res) = masterPlaylist
      HLS.playlistType res `shouldBe` HLS.MasterPlaylistType

    it "parses a media playlist" $ do
      mediaPlaylist <- openFixturePlaylist "media-playlist.m3u8"

      mediaPlaylist `shouldSatisfy` Either.isRight
      unsafeFromRight mediaPlaylist `shouldView` HLS.HLSVersion 3 `through` HLS.hlsVersion
      HLS.playlistType (unsafeFromRight mediaPlaylist) `shouldBe` HLS.MediaPlaylistType

    it "extracts multiple tags" $ do
      playlist <- openFixturePlaylist "master-playlist-multiple-tags.m3u8"

      playlist `shouldSatisfy` Either.isRight
      let (Right res) = playlist
      let (Just uri) = HLS.HLSURI <$> URI.parseAbsoluteURI "https://example.net/hls/live/12345/snappyTV/stream1/PassyNetwork_Snap_1_a.m3u8"
      let entries = (uri, ["-X-CUSTOM:FOO", "-X-CROSSFIT:WACK"]) :: HLS.HLSEntry
      res `shouldView` pure entries `through` HLS.hlsEntries

openFixturePlaylist :: MonadIO m => FilePath -> m (Either (M.ParseError Char M.Dec) HLS.HLSPlaylist)
openFixturePlaylist = liftIO . fmap HLS.parseHlsPlaylist . openTextFixture

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right a) = a
unsafeFromRight (Left _) = error "Left"
