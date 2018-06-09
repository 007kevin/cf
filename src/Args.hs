module Args where

import Options.Applicative
import Data.Monoid
import Login (initLogin)
import Submit (initSubmit)
import Metadata (appVersion)

data Args = Args Command
  deriving Show

data Command
  = Login LoginOpts
  | Submit SubmitOpts
  deriving Show

data LoginOpts = LoginOpts
  { username :: String }
  deriving Show

loginParser :: Parser Command
loginParser = Login <$> loginOpts
  
loginOpts :: Parser LoginOpts
loginOpts = LoginOpts <$> argument str (metavar "HANDLE")

data SubmitOpts = SubmitOpts
  { file :: String,
    lang :: Maybe String,
    prob :: Maybe String }
  deriving Show

submitParser :: Parser Command
submitParser = Submit <$> submitOpts

submitOpts :: Parser SubmitOpts
submitOpts = SubmitOpts
               <$> argument str (metavar "FILE")
               <*> (optional $ strOption ( long "lang" <>
                               metavar "LANG" <>
                               help "Language for submission" ))
               <*> (optional $ strOption ( long "prob" <>
                               metavar "PROB" <>
                               help "Codeforces problem" ))

version :: Parser (a -> a)
version = infoOption appVersion
  ( long "version" <>
    help "Print version information" )

parser :: Parser Args
parser = Args <$>
  subparser ( command "login"
              ( info loginParser
                ( progDesc "Log into Codeforces" )) <>
              command "submit"
              ( info submitParser
                ( progDesc "Submit a problem to Codeforces" )))

opts = info ( helper <*> version <*> parser ) idm

run :: Args -> IO()
run (Args (Login (LoginOpts handle)))            = initLogin handle
run (Args (Submit (SubmitOpts file lang prob ))) =
  putStrLn $ unwords [file, (show lang), (show prob)]

start :: IO ()
start = execParser opts >>= run
  
  
