{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | A very, very incomplete parser for HLS draft 19, version 3.
module Lib.HLS.Parse where

import           Control.Lens          (makeLenses)
import           Control.Monad         (void)
import           Data.Monoid           ((<>))
import qualified Network.URI as URI
import qualified Data.Text             as T
import qualified Text.Megaparsec       as M
import qualified Text.Megaparsec.Lexer as M
import qualified Text.Megaparsec.Text  as M

import Data.Maybe (fromJust)

maxSupportedVersionNumber :: Int
maxSupportedVersionNumber = 3

newtype HLSVersion = HLSVersion Int
  deriving (Eq, Show)

newtype HLSURI = HLSURI URI.URI
  deriving (Eq, Show)

data HLSTag = HLSTag
  deriving (Eq, Show)

data HLSPlaylist = HLSPlaylist
  { _hlsVersion :: HLSVersion
  , _hlsEntries :: [(HLSURI, [HLSTag])]
  } deriving (Eq, Show)

makeLenses ''HLSPlaylist

ext :: M.Parser ()
ext = void $ M.string "#EXT"

extx :: String -> M.Parser ()
extx s = ext *> void (M.string ("-X-" <> s <> ":"))

extm3u :: M.Parser ()
extm3u = ext *> void (M.string "M3U") *> void M.newline

versionParser :: M.Parser HLSVersion
versionParser = do
  extx "VERSION"
  v <- M.hidden $ fromInteger <$> M.integer -- Ignoring potential overflow
  if v <= maxSupportedVersionNumber
    then return $ HLSVersion v
    else M.unexpected $ "Unsupported version " <> show v


entryParser :: M.Parser (HLSURI, [HLSTag])
entryParser =
  (,) <$> uriParser <*> M.some tagParser
  where
    tagParser = ext <* M.skipSome M.anyChar <* M.eol *> pure HLSTag
    uriParser = pure . HLSURI . fromJust $ URI.parseURI "https://example.com/path.m3u8"

hlsPlaylistParser :: M.Parser HLSPlaylist
hlsPlaylistParser = do
  extm3u
  _hlsVersion <- versionParser

  return HLSPlaylist { .. }

parseHlsPlaylist :: T.Text -> Either M.ParseError HLSPlaylist
parseHlsPlaylist = M.parse hlsPlaylistParser ""
