{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

import           Control.Lens                 (to)
import           Control.Monad.IO.Class       (liftIO, MonadIO)
import qualified Data.Either.Combinators      as Either
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as TIO
import qualified Lib.HLS.Parse                as HLS
import           System.Directory             (getCurrentDirectory)
import           System.FilePath              ((</>))
import           Test.Hspec
import           Test.Hspec.Expectations.Lens (shouldView, through)
import qualified Text.Megaparsec              as M

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
      M.errorMessages (Either.fromLeft mempty masterPlaylist) `shouldBe` pure (M.Unexpected "Unsupported version 88")

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

openFixturePlaylist :: MonadIO m => FilePath -> m (Either M.ParseError HLS.HLSPlaylist)
openFixturePlaylist = liftIO . fmap HLS.parseHlsPlaylist . openTextFixture

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right a) = a
unsafeFromRight (Left _) = error "Left"
