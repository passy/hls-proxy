{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Console where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (modifyTVar, readTVar)
import           Control.Monad               (forever)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           System.Exit                 (exitSuccess)
import qualified System.IO                   as IO

import           Control.Lens                ((^.))
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

interpret :: RuntimeOptions -> CLICommand -> IO ()
interpret ropts = \case
  CmdQuit -> exitSuccess
  CmdToggleEmpty -> atomically $ modifyTVar (ropts ^. enableEmptyPlaylist) not
  CmdShow -> TIO.putStrLn =<< atomically (showRuntimeOptions ropts)
  CmdUnknown -> TIO.putStrLn "!! Unknown Command"

consoleThread :: RuntimeOptions -> IO ()
consoleThread ropts = forever $ do
  putStr "> "
  IO.hFlush IO.stdout
  interpret ropts =<< parseCLICommand <$> TIO.getLine
