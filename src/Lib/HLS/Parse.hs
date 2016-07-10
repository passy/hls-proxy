{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | A very, very incomplete parser for HLS draft 19, version 3.
module Lib.HLS.Parse where

import           Control.Lens          (makeLenses, to, (^.), _1)
import           Control.Monad         (void)
import qualified Data.List             as List
import           Data.Monoid           ((<>))
import           Data.String           (IsString (..))
import qualified Data.Text             as T
import           Lib.Types             (ParseError)
import qualified Network.URI           as URI
import qualified Text.Megaparsec       as M
import qualified Text.Megaparsec.Lexer as L
import qualified Text.Megaparsec.Text  as M

maxSupportedVersionNumber :: Int
maxSupportedVersionNumber = 3

newtype HLSVersion = HLSVersion Int
  deriving (Eq, Show)

newtype HLSURI = HLSURI { _hlsURI :: URI.URI }
  deriving (Eq, Show)

makeLenses ''HLSURI

-- TODO: Temporary way to store the tag without losing information.
data HLSTag = HLSTag T.Text
  deriving (Eq, Show)

instance IsString HLSTag where
  fromString = HLSTag . T.pack

data PlaylistType = MasterPlaylistType | MediaPlaylistType
  deriving (Eq, Show)

type HLSEntry = (HLSURI, [HLSTag])

data HLSPlaylist = HLSPlaylist
  { _hlsVersion :: HLSVersion
  , _hlsEntries :: [HLSEntry]
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
    else fail $ "Unsupported version " <> show v

maybeParse :: String -> Maybe a -> M.Parser a
maybeParse tag = \case
  Just a  -> pure a
  Nothing -> fail $ "Unexpected " <> tag

entryParser :: M.Parser HLSEntry
entryParser = do
  tags <- M.many tagParser
  M.skipMany M.spaceChar
  uri <- uriParser
  return (uri, tags)
  where
    tagParser :: M.Parser HLSTag
    tagParser = HLSTag . T.pack <$> (ext *> M.someTill M.printChar M.newline)

    uriParser :: M.Parser HLSURI
    uriParser = do
      mayUri <- URI.parseURI <$> M.someTill M.printChar M.newline
      uri <- maybeParse "invalid URI" mayUri
      return $ HLSURI uri

hlsPlaylistParser :: M.Parser HLSPlaylist
hlsPlaylistParser = do
  extm3u <* M.newline
  _hlsVersion <- versionParser <* M.newline
  M.space
  _hlsEntries <- M.some $ entryParser <* M.space

  return HLSPlaylist { .. }

parseHlsPlaylist :: T.Text -> Either ParseError HLSPlaylist
parseHlsPlaylist = M.parse (hlsPlaylistParser <* M.eof) ""

playlistType :: HLSPlaylist -> PlaylistType
playlistType pl
  | any (hasExt ".ts") (pl ^. hlsEntries) = MediaPlaylistType
  | otherwise = MasterPlaylistType
  where
    hasExt :: String -> HLSEntry -> Bool
    hasExt ext' entry =
      let path = entry ^. _1 . hlsURI . to URI.uriPath
      in  ext' `List.isSuffixOf` path
