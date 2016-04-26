{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Server
    ( server
    , ServerOptions(..)
    , Port(..)
    , url
    , port
    ) where

import           Control.Lens                         (makeLenses, (&), (^.))
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as B8
import           Data.CaseInsensitive                 (CI)
import           Data.Default                         (Default (), def)
import           Data.Monoid                          ((<>))
import qualified Data.Text                            as T
import qualified Network.HTTP.Conduit                 as Conduit
import qualified Network.HTTP.ReverseProxy            as Proxy
import qualified Network.HTTP.Types.Header            as Header
import qualified Network.HTTP.Types.Status            as Status
import qualified Network.Wai                          as Wai
import           Network.Wai.Handler.Warp             (runEnv)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Safe                                 (lastMay)
import           System.FilePath                      ((</>))

import           Paths_hls_proxy                      (getDataFileName)

newtype Port = Port Int
  deriving (Read, Show)

unPort :: Port -> Int
unPort (Port i) = i

-- | Command line options provided to start up the server.
data ServerOptions = ServerOptions
  { _url   :: String
  , _port  :: Port
  , _empty :: Bool
  }

makeLenses ''ServerOptions

instance Default Port where
  def = Port 3000

hostHeader :: BS.ByteString -> (CI BS.ByteString, BS.ByteString)
hostHeader dest = ("Host", dest)

setHostHeaderToDestination :: BS.ByteString -> Wai.Request -> Wai.Request
setHostHeaderToDestination dest req =
  req { Wai.requestHeaders = replaceHostHeader (Wai.requestHeaders req) }
  where replaceHostHeader =
          map (\header@(name, _) ->
            case name of
              "Host" -> hostHeader dest
              _      -> header)

server :: ServerOptions -> IO ()
server opts = do
  manager <- Conduit.newManager Conduit.tlsManagerSettings
  runEnv (opts ^. port & unPort) (logStdout $ proxy opts manager)

data PlaylistType = MasterPlaylist | MediaPlaylist | InvalidPlaylist

-- | Based on a very rough heuristic only working in our particular setup
-- that *will* break.
playlistType :: Wai.Request -> PlaylistType
playlistType r =
  case T.isInfixOf "x" <$> lastMay (Wai.pathInfo r) of
    Just True -> MediaPlaylist
    Just False -> MasterPlaylist
    Nothing -> InvalidPlaylist

getDataM3U8 :: FilePath -> IO FilePath
getDataM3U8 fp =
  getDataFileName $ "shared" </> "m3u8" </> fp

hlsHeaders :: Header.RequestHeaders
hlsHeaders = pure (Header.hContentType, "application/x-mpegURL")

mkEmptyEndedPlaylistResponse :: IO Wai.Response
mkEmptyEndedPlaylistResponse = do
  path <- getDataM3U8 "event_empty_ended.m3u8"
  return $ Wai.responseFile Status.status200 (def <> hlsHeaders) path Nothing

passthrough :: B8.ByteString -> Wai.Request -> IO Proxy.WaiProxyResponse
passthrough dest req =
  return $ Proxy.WPRModifiedRequestSecure
    (setHostHeaderToDestination dest req)
    (Proxy.ProxyDest dest 443)

proxy :: ServerOptions -> Conduit.Manager -> Wai.Application
proxy opts =
  if opts ^. empty then
    Proxy.waiProxyToSettings transform def
  else
    Proxy.waiProxyToSettings noopTransform def
  where
    destBS =
      opts ^. url & B8.pack
    noopTransform = passthrough destBS
    transform req =
      let pt = playlistType req
      in case pt of
        MediaPlaylist ->
          mkEmptyEndedPlaylistResponse >>= pure . Proxy.WPRResponse
        _             ->
          passthrough destBS req
