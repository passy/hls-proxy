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
import qualified Control.Concurrent.MVar              as MVar
import           Control.Concurrent.STM               (atomically)
import           Control.Concurrent.STM.TVar          (TVar, newTVarIO,
                                                       readTVar)
import           Control.Lens                         ((&), (^.))
import           Control.Monad                        (void)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as B8
import qualified Data.ByteString.Lazy                 as BL
import           Data.CaseInsensitive                 (CI)
import           Data.Default                         (def)
import           Data.Monoid                          ((<>))
import qualified Data.Text                            as T
import qualified Network.HTTP.Conduit                 as Conduit
import qualified Network.HTTP.ReverseProxy            as Proxy
import qualified Network.HTTP.Types.Header            as Header
import qualified Network.HTTP.Types.Status            as Status
import qualified Network.URI                          as URI
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Safe                                 (lastMay)

import           Lib.Console                          (consoleThread)
import qualified Lib.Files                            as Files
import           Lib.Types                            (Port,
                                                       RuntimeOptions (..), ServerOptions (ServerOptions),
                                                       defRuntimeOptions,
                                                       enableEmptyPlaylist,
                                                       overrideMasterPlaylist,
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

mkSettings :: MVar.MVar () -> ServerOptions -> Warp.Settings
mkSettings poison sopts =
  Warp.setPort (sopts ^. port & unPort) . Warp.setInstallShutdownHandler (void . shutdownHandler) $ Warp.defaultSettings
  where shutdownHandler cb = forkIO $ MVar.takeMVar poison >> cb

server :: ServerOptions -> IO ()
server sopts = do
  manager <- Conduit.newManager Conduit.tlsManagerSettings
  ropts <- newTVarIO defRuntimeOptions
  poison <- MVar.newEmptyMVar
  _ <- forkIO $ consoleThread poison ropts
  Warp.runSettings (mkSettings poison sopts) (logStdout $ proxy ropts sopts manager)

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

passthrough :: B8.ByteString -> Wai.Request -> Proxy.WaiProxyResponse
passthrough dest req =
  Proxy.WPRModifiedRequestSecure
    (setHostHeaderToDestination dest req)
    (Proxy.ProxyDest dest 443)

respondEmptyEndedPlaylist :: Proxy.WaiProxyResponse
respondEmptyEndedPlaylist = Proxy.WPRResponse mkEmptyEndedPlaylistResponse

proxy :: TVar RuntimeOptions -> ServerOptions -> Conduit.Manager -> Wai.Application
proxy ropts sopts =
  Proxy.waiProxyToSettings transform def
  where
    destBS =
      sopts ^. url & B8.pack

    transform req = do
      opts <- atomically $ readTVar ropts
      case playlistType req of
        MediaPlaylist -> mediaHandler opts req
        _             -> masterHandler opts req

    masterHandler opts req =
      case opts ^. overrideMasterPlaylist of
        Just o -> return $ redirect o destBS req
        Nothing -> emptyPlaylistHandler opts req

    emptyPlaylistHandler opts req =
      if opts ^. enableEmptyPlaylist
        then return respondEmptyEndedPlaylist
        else return $ passthrough destBS req

    mediaHandler _ =
      return . passthrough destBS

redirect :: URI.URI -> B8.ByteString -> Wai.Request -> Proxy.WaiProxyResponse
redirect override dest req =
  -- TODO: Make lenses for Wai.*
  let regName = maybe mempty (B8.pack . URI.uriRegName) $ URI.uriAuthority override
      req' = req { Wai.rawPathInfo = B8.pack $ URI.uriPath override }
      req'' = setHostHeaderToDestination regName req'
  in case URI.uriScheme override of
    "http:"  -> Proxy.WPRModifiedRequest req'' (Proxy.ProxyDest dest 80)
    "https:" -> Proxy.WPRModifiedRequestSecure req'' (Proxy.ProxyDest dest 443)
    _        -> Proxy.WPRResponse $ Wai.responseLBS Status.status501 [] "Unsupported override host scheme."
