module Main where

import           Control.Applicative      ((<**>))
import           Data.Default             (def)
import           Data.Monoid              ((<>))
import           Data.Version             (Version (), showVersion)
import           Lib.Server               (server)
import           Lib.Types                (Port (..), ServerOptions (..))
import qualified Options.Applicative      as Opt
import qualified Options.Applicative.Text as Opt
import           Paths_hls_proxy          (version)
import           System.Environment       (getProgName)


readPort :: Opt.ReadM Port
readPort = Opt.eitherReader $ \arg -> case reads arg of
  [(r, "")] -> return $ Port r
  _         -> Left $ "cannot parse port value `" <> arg <> "'"

serverParser :: String -> Version -> Opt.ParserInfo ServerOptions
serverParser progName ver =
  Opt.info ( Opt.helper <*> appOpts <**> versionInfo )
    ( Opt.fullDesc
   <> Opt.progDesc "Modify HLS streams in-flight."
   <> Opt.header progName )
  where
    appOpts = ServerOptions
      <$> Opt.textArgument (Opt.metavar "URL")
      <*> Opt.option readPort
          ( Opt.long "port"
         <> Opt.short 'p'
         <> Opt.value def
         <> Opt.showDefault
         <> Opt.help "Port" )

    versionInfo = Opt.infoOption ( progName <> showVersion ver )
      ( Opt.short 'V'
     <> Opt.long "version"
     <> Opt.hidden
     <> Opt.help "Show version information" )

main :: IO ()
main = do
  progName <- getProgName
  Opt.execParser (serverParser progName version) >>= server
