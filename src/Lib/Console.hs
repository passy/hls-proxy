{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Console where

import           Control.Applicative         ((<|>))
import qualified Control.Concurrent.MVar     as MVar
import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar)
import           Control.Monad               (forever)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import qualified Network.URI                 as URI
import           System.Exit                 (exitSuccess)
import qualified System.IO                   as IO

import           Control.Lens                (over, set)
import           Lib.Types                   (RuntimeOptions,
                                              ParseError,
                                              enableEmptyPlaylist,
                                              overrideMasterPlaylist,
                                              showRuntimeOptions)

import qualified Text.Megaparsec             as M
import qualified Text.Megaparsec.Text        as M

data CLICommand = CmdQuit
                | CmdToggleEmpty
                | CmdShow
                | CmdOverride (Maybe URI.URI)
                | CmdUnknown (Maybe ParseError)
  deriving (Show, Eq)

cliCommandParser :: M.Parser CLICommand
cliCommandParser =
        p "quit" CmdQuit
    <|> p "empty" CmdToggleEmpty
    <|> p "show" CmdShow
    <|> overrideParser
  where
    p :: String -> a -> M.Parser a
    p []          _   =
      fail "Invalid CLI pattern."
    p token@(t:_) cmd =
      (M.try (M.char t *> M.eof) <|> (M.string token *> M.eof)) *> pure cmd

    overrideParser :: M.Parser CLICommand
    overrideParser = do
      _ <- M.try (M.char 'o' *> M.space) <|> (M.string "override" *> M.space)
      CmdOverride . URI.parseURI <$> M.manyTill M.printChar M.eof

parseCLICommand :: T.Text -> CLICommand
parseCLICommand input = case M.parse cliCommandParser "<input>" input of
  Right cmd -> cmd
  Left  err -> CmdUnknown $ pure err

interpret :: MVar.MVar () -> TVar RuntimeOptions -> CLICommand -> IO ()
interpret poison ropts = \case
  CmdQuit -> MVar.putMVar poison () >> exitSuccess
  CmdToggleEmpty -> atomically $ modifyTVar ropts (over enableEmptyPlaylist not)
  CmdOverride o -> atomically $ modifyTVar ropts (set overrideMasterPlaylist o)
  CmdShow -> TIO.putStrLn =<< atomically (showRuntimeOptions ropts)
  CmdUnknown Nothing -> TIO.putStrLn "!! Unknown Command"
  CmdUnknown (Just err) -> TIO.putStrLn $ "!! Error: " <> T.pack (show err)

consoleThread :: MVar.MVar () -> TVar RuntimeOptions -> IO ()
consoleThread poison ropts = forever $ do
  putStr "> "
  IO.hFlush IO.stdout
  interpret poison ropts =<< parseCLICommand <$> TIO.getLine
