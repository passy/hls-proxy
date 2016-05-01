{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Console where

import           Control.Concurrent.STM      (atomically)
import qualified Control.Concurrent.MVar as MVar
import           Control.Concurrent.STM.TVar (TVar, modifyTVar, readTVar)
import           Control.Monad               (forever)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           System.Exit                 (exitSuccess)
import qualified System.IO                   as IO

import           Control.Lens                (set, over, (^.))
import           Lib.Types                   (RuntimeOptions,
                                              enableEmptyPlaylist,
                                              showRuntimeOptions)

data CLICommand = CmdQuit | CmdToggleEmpty | CmdShow | CmdUnknown
  deriving (Show, Eq)

parseCLICommand :: T.Text -> CLICommand
parseCLICommand input = case T.toLower input of
  "q" -> CmdQuit
  "quit" -> CmdQuit
  "e" -> CmdToggleEmpty
  "empty" -> CmdToggleEmpty
  "s" -> CmdShow
  "show" -> CmdShow
  _ -> CmdUnknown

interpret :: MVar.MVar () -> TVar RuntimeOptions -> CLICommand -> IO ()
interpret poison ropts = \case
  CmdQuit -> MVar.putMVar poison () >> exitSuccess
  CmdToggleEmpty -> atomically $ modifyTVar ropts (over enableEmptyPlaylist not)
  CmdShow -> TIO.putStrLn =<< atomically (showRuntimeOptions ropts)
  CmdUnknown -> TIO.putStrLn "!! Unknown Command"

consoleThread :: MVar.MVar () -> TVar RuntimeOptions -> IO ()
consoleThread poison ropts = forever $ do
  putStr "> "
  IO.hFlush IO.stdout
  interpret poison ropts =<< parseCLICommand <$> TIO.getLine
