{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Types where

import           Control.Concurrent.STM      (STM ())
import           Control.Concurrent.STM.TVar (TVar (), newTVar, readTVar)
import           Control.Lens                (makeLenses, (^.))
import           Data.Default                (Default (), def)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T

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
  _enableEmptyPlaylist <- newTVar False
  return RuntimeOptions {..}

tshow :: Show a => a -> T.Text
tshow = T.pack . show

showRuntimeOptions :: RuntimeOptions -> STM T.Text
showRuntimeOptions ropts = do
  _enableEmptyPlaylist <- readTVar $ ropts ^. enableEmptyPlaylist
  return $ "{ enableEmptyPlaylist = " <> tshow _enableEmptyPlaylist <> " }"

instance Default Port where
  def = Port 3000
