{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Server
    ( server
    , ServerOptions(..)
    , Port(..)
    , url
    , port
    ) where


import qualified Web.Scotty   as S

import           Control.Lens
import           Data.Default (Default (), def)

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

server :: ServerOptions -> IO ()
server opts =
  S.scotty (unPort $ opts ^. port) $
    S.get "/live/feed.m3u8" $ do
      S.setHeader "Content-Type" "application/x-mpegURL"
      S.text "Hello, World!"
