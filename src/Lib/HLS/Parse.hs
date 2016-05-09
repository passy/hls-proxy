{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | A very, very incomplete parser for HLS draft 19, version 3.
module Lib.HLS.Parse where

-- TODO: Reexport M.ParseError

import           Control.Lens          (makeLenses)
import           Control.Monad         (void)
import           Data.Monoid           ((<>))
import qualified Network.URI as URI
import qualified Data.Text             as T
import qualified Text.Megaparsec       as M
import qualified Text.Megaparsec.Lexer as L
import qualified Text.Megaparsec.Text  as M

import Data.Maybe (fromJust)

maxSupportedVersionNumber :: Int
maxSupportedVersionNumber = 3

newtype HLSVersion = HLSVersion Int
  deriving (Eq, Show)

-- TODO: Temporary Maybe until this is joined into the ParserT.
newtype HLSURI = HLSURI (Maybe URI.URI)
  deriving (Eq, Show)

-- TODO: Temporary way to store the tag without losing information.
data HLSTag = HLSTag T.Text
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
extm3u = ext *> void (M.string "M3U")

versionParser :: M.Parser HLSVersion
versionParser = do
  extx "VERSION"
  v <- M.hidden $ fromInteger <$> L.integer -- Ignoring potential overflow
  if v <= maxSupportedVersionNumber
    then return $ HLSVersion v
    else M.unexpected $ "Unsupported version " <> show v

entryParser :: M.Parser (HLSURI, [HLSTag])
entryParser =
  flip (,) <$> M.sepEndBy1 tagParser M.newline <*> uriParser
  where
    tagParser = HLSTag . T.pack <$> (ext *> M.someTill M.printChar M.newline)
    uriParser = HLSURI . URI.parseURI <$> M.someTill M.printChar M.newline

hlsPlaylistParser :: M.Parser HLSPlaylist
hlsPlaylistParser = do
  extm3u <* M.newline
  _hlsVersion <- versionParser <* M.newline
  _hlsEntries <- M.some entryParser
  -- TODO: Skip empty lines so I can match against EOF.

  return HLSPlaylist { .. }

parseHlsPlaylist :: T.Text -> Either M.ParseError HLSPlaylist
parseHlsPlaylist = M.parse hlsPlaylistParser ""
