{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Types where

import           Control.Concurrent.STM      (STM (), atomically)
import           Control.Concurrent.STM.TVar (TVar (), newTVar)
import           Control.Lens                (makeLenses)
import           Data.Default                (Default (), def)

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
  { _enableEmptyPlaylist :: TVar Bool
  }

makeLenses ''RuntimeOptions

defRuntimeOptions :: STM RuntimeOptions
defRuntimeOptions = do
  _serveEmptyPlaylist <- newTVar False
  return RuntimeOptions {..}

instance Default Port where
  def = Port 3000
