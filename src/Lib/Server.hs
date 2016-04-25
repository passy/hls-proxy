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

import           Control.Lens                         ((^.), (&), makeLenses)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as B8
import           Data.CaseInsensitive                 (CI)
import           Data.Default                         (Default (), def)
import qualified Network.HTTP.Conduit                 as Conduit
import           Network.HTTP.ReverseProxy            (ProxyDest (ProxyDest),
                                                       WaiProxyResponse (..),
                                                       waiProxyToSettings)
import qualified Network.Wai                          as Wai
import           Network.Wai.Handler.Warp             (runEnv)
import           Network.Wai.Middleware.RequestLogger (logStdout)

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

proxy :: ServerOptions -> Conduit.Manager -> Wai.Application
proxy opts = waiProxyToSettings transform def
  where
    destBS =
      opts ^. url & B8.pack
    transform req =
      return $ WPRModifiedRequestSecure (setHostHeaderToDestination destBS req) (ProxyDest destBS 443)
