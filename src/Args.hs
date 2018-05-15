module Args where

import Options.Applicative
import Data.Monoid
import Login

data Args = Args Command
  deriving Show

data Command
  = Login LoginOpts
  deriving Show

data LoginOpts = LoginOpts
  { username :: String }
  deriving Show

version :: Parser (a -> a)
version = infoOption "0.0.0"
  ( long "version" <>
    help "Print version information" )

loginParser :: Parser Command
loginParser = Login <$> loginOpts
  
loginOpts :: Parser LoginOpts
loginOpts = LoginOpts <$> argument str (metavar "HANDLE")

parser :: Parser Args
parser = Args <$>
  subparser ( command "login"
              ( info loginParser
                ( progDesc "Log into Codeforces" )))

opts = info ( helper <*> version <*> parser ) idm

run :: Args -> IO()
run (Args (Login (LoginOpts handle))) = initLogin handle

start :: IO ()
start = execParser opts >>= run
  
  
