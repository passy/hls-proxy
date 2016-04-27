{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Server
    ( server
    , ServerOptions(..)
    , Port()
    , url
    , port
    ) where

import           Control.Concurrent                   (forkIO)
import           Control.Concurrent.STM               (atomically)
import           Control.Concurrent.STM.TVar          (readTVar)
import           Control.Lens                         ((&), (^.))
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as BL
import qualified Data.ByteString.Char8                as B8
import           Data.CaseInsensitive                 (CI)
import           Data.Default                         (def)
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

import           Lib.Console                          (consoleThread)
import qualified Lib.Files                            as Files
import           Lib.Types                            (Port,
                                                       RuntimeOptions (..), ServerOptions (ServerOptions),
                                                       defRuntimeOptions,
                                                       enableEmptyPlaylist,
                                                       port, unPort, url)

hostHeader :: BS.ByteString -> (CI BS.ByteString, BS.ByteString)
hostHeader dest = ("Host", dest)

setHostHeaderToDestination :: BS.ByteString -> Wai.Request -> Wai.Request
setHostHeaderToDestination dest req =
  req { Wai.requestHeaders = replaceHostHeader (Wai.requestHeaders req) }
  where replaceHostHeader =
          fmap (\header@(name, _) ->
            case name of
              "Host" -> hostHeader dest
              _      -> header)

server :: ServerOptions -> IO ()
server sopts = do
  manager <- Conduit.newManager Conduit.tlsManagerSettings
  ropts <- atomically defRuntimeOptions
  _ <- forkIO $ consoleThread ropts
  runEnv (sopts ^. port & unPort) (logStdout $ proxy ropts sopts manager)

data PlaylistType = MasterPlaylist | MediaPlaylist | InvalidPlaylist

-- | Based on a very rough heuristic only working in our particular setup
-- that *will* break.
playlistType :: Wai.Request -> PlaylistType
playlistType r =
    case T.isInfixOf "x" <$> lastMay (Wai.pathInfo r) of
    Just True -> MediaPlaylist
    Just False -> MasterPlaylist
    Nothing -> InvalidPlaylist

hlsHeaders :: Header.RequestHeaders
hlsHeaders = pure (Header.hContentType, "application/x-mpegURL")

mkEmptyEndedPlaylistResponse :: Wai.Response
mkEmptyEndedPlaylistResponse =
  Wai.responseLBS Status.status200 (def <> hlsHeaders) (BL.fromStrict Files.eventEmptyEndedM3U8File)

passthrough :: B8.ByteString -> Wai.Request -> IO Proxy.WaiProxyResponse
passthrough dest req =
  return $ Proxy.WPRModifiedRequestSecure
    (setHostHeaderToDestination dest req)
    (Proxy.ProxyDest dest 443)

respondEmptyEndedPlaylist :: Proxy.WaiProxyResponse
respondEmptyEndedPlaylist = Proxy.WPRResponse mkEmptyEndedPlaylistResponse

proxy :: RuntimeOptions -> ServerOptions -> Conduit.Manager -> Wai.Application
proxy ropts sopts =
  Proxy.waiProxyToSettings transform def
  where
    destBS =
      sopts ^. url & B8.pack
    transform req = do
      empty <- atomically . readTVar $ ropts ^. enableEmptyPlaylist
      let pt = playlistType req
      case pt of
        MediaPlaylist -> if empty then pure respondEmptyEndedPlaylist else passthrough destBS req
        _             -> passthrough destBS req
