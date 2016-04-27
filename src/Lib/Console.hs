{-# LANGUAGE OverloadedStrings #-}

module Lib.Console where

import           Control.Concurrent.STM      (STM (), atomically)
import           Control.Concurrent.STM.TVar (readTVar)
import           Control.Monad               (forever)
import           Data.Default                (Default (), def)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import qualified System.IO                   as IO

import Control.Lens ((^.))
import           Lib.Types                   (RuntimeOptions,
                                              enableEmptyPlaylist)

data CLICommand = CmdQuit | CmdToggleEmpty | CmdUnknown
  deriving (Show, Eq)

parseCLICommand :: T.Text -> CLICommand
parseCLICommand input = case T.toLower input of
  "q" -> CmdQuit
  "quit" -> CmdQuit
  "e" -> CmdToggleEmpty
  "empty" -> CmdToggleEmpty
  _ -> CmdUnknown

consoleThread :: RuntimeOptions -> IO ()
consoleThread ropts = forever $ do
  putStr "> "
  IO.hFlush IO.stdout
  cmd <- parseCLICommand <$> TIO.getLine
  empty <- atomically . readTVar $ ropts ^. enableEmptyPlaylist
  print cmd
