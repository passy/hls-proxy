{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Server
    ( server
    , ServerOptions(..)
    , Port(..)
    , url
    , port
    ) where

import           Control.Concurrent                   (forkIO)
import           Control.Concurrent.STM               (STM (), atomically)
import           Control.Concurrent.STM.TVar          (TVar (), newTVar,
                                                       readTVar)
import           Control.Lens                         (makeLenses, (&), (^.))
import           Control.Monad                        (forever)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as B8
import           Data.CaseInsensitive                 (CI)
import           Data.Default                         (Default (), def)
import           Data.Monoid                          ((<>))
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as TIO
import qualified Network.HTTP.Conduit                 as Conduit
import qualified Network.HTTP.ReverseProxy            as Proxy
import qualified Network.HTTP.Types.Header            as Header
import qualified Network.HTTP.Types.Status            as Status
import qualified Network.Wai                          as Wai
import           Network.Wai.Handler.Warp             (runEnv)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Safe                                 (lastMay)
import           System.FilePath                      ((</>))
import qualified System.IO as IO

import           Paths_hls_proxy                      (getDataFileName)

newtype Port = Port Int
  deriving (Read, Show)

unPort :: Port -> Int
unPort (Port i) = i

-- | Command line options provided to start up the server.
data ServerOptions = ServerOptions
  { _url  :: String
  , _port :: Port
  }

makeLenses ''ServerOptions

data RuntimeOptions = RuntimeOptions
  { _serveEmptyPlaylist :: TVar Bool
  }

makeLenses ''RuntimeOptions

defRuntimeOptions :: STM RuntimeOptions
defRuntimeOptions = do
  _serveEmptyPlaylist <- newTVar False
  return RuntimeOptions {..}

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
server sopts = do
  manager <- Conduit.newManager Conduit.tlsManagerSettings
  ropts <- atomically defRuntimeOptions
  _ <- forkIO $ console ropts
  runEnv (sopts ^. port & unPort) (logStdout $ proxy ropts sopts manager)

data CLICommand = CmdQuit | CmdToggleEmpty | CmdUnknown
  deriving (Show, Eq)

parseCLICommand :: T.Text -> CLICommand
parseCLICommand input = case T.toLower input of
  "q" -> CmdQuit
  "quit" -> CmdQuit
  "e" -> CmdToggleEmpty
  "empty" -> CmdToggleEmpty
  _ -> CmdUnknown

console :: RuntimeOptions -> IO ()
console ropts = forever $ do
  putStr "> "
  IO.hFlush IO.stdout
  cmd <- parseCLICommand <$> TIO.getLine
  empty <- atomically . readTVar $ ropts ^. serveEmptyPlaylist
  print cmd

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

respondEmptyEndedPlaylist :: IO Proxy.WaiProxyResponse
respondEmptyEndedPlaylist = mkEmptyEndedPlaylistResponse >>= pure . Proxy.WPRResponse

proxy :: RuntimeOptions -> ServerOptions -> Conduit.Manager -> Wai.Application
proxy ropts sopts =
  Proxy.waiProxyToSettings transform def
  where
    destBS =
      sopts ^. url & B8.pack
    transform req = do
      empty <- atomically . readTVar $ ropts ^. serveEmptyPlaylist
      let pt = playlistType req
      case pt of
        MediaPlaylist -> if empty then respondEmptyEndedPlaylist else passthrough destBS req
        _             -> passthrough destBS req
