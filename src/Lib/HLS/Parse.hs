{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.HLS.Parse where

import           Control.Lens          (makeLenses)
import           Control.Monad         (void)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Text.Megaparsec       as M
import qualified Text.Megaparsec.Lexer as M
import qualified Text.Megaparsec.Text  as M

data HLSPlaylist = HLSPlaylist
  { _hlsVersion :: Int
  } deriving (Eq, Show)

makeLenses ''HLSPlaylist

ext :: M.Parser ()
ext = void $ M.string "#EXT"

extx :: String -> M.Parser ()
extx s = ext *> void (M.string ("-X-" <> s <> ":"))

extm3u :: M.Parser ()
extm3u = ext *> void (M.string "M3U") *> void M.newline

hlsPlaylistParser :: M.Parser HLSPlaylist
hlsPlaylistParser = do
  extm3u
  extx "VERSION"
  _hlsVersion <- fromInteger <$> M.integer -- Ignoring potential overflow

  return HLSPlaylist { .. }

parseHlsPlaylist :: T.Text -> Either M.ParseError HLSPlaylist
parseHlsPlaylist = M.parse hlsPlaylistParser ""
