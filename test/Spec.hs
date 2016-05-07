{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

import           Control.Monad.IO.Class       (liftIO)
import qualified Data.Either                  as Either
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as TIO
import qualified Lib.HLS.Parse                as HLS
import           System.Directory             (getCurrentDirectory)
import           System.FilePath              ((</>))
import           Test.Hspec
import           Test.Hspec.Expectations.Lens (shouldView, through)

openFixture :: forall a. (FilePath -> IO a) -> FilePath -> IO a
openFixture f path = do
    dir <- getCurrentDirectory
    f $ dir </> "test" </> "fixtures" </> path

openTextFixture :: FilePath -> IO T.Text
openTextFixture = openFixture TIO.readFile

main :: IO ()
main = hspec .
  describe "HLS Parser" .
    it "read master playlist version" $ do
    doc <- liftIO $ openTextFixture "master-playlist.m3u8"
    let res = HLS.parseHlsPlaylist doc

    res `shouldSatisfy` Either.isRight
    unsafeFromRight res `shouldView` 3 `through` HLS.hlsVersion

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right r) = r
unsafeFromRight (Left _)  = error "Left"
