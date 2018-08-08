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
  { file :: Maybe String,
    cont :: Maybe String,    
    prob :: Maybe String,
    lang :: Maybe String }
  deriving Show

submitParser :: Parser Command
submitParser = Submit <$> submitOpts

submitOpts :: Parser SubmitOpts
submitOpts = SubmitOpts
             <$> (optional$ argument str (metavar "FILE"))
             <*> (optional $ strOption ( long "prob" <>
                                         metavar "PROB" <>
                                         help "Codeforces problem index, eg A" ))
             <*> (optional $ strOption ( long "cont" <>
                                         metavar "CONT" <>
                                         help "Codeforces contest, eg 100" ))
             <*> (optional $ strOption ( long "lang" <>
                                         metavar "LANG" <>
                                         help "Language for submission" ))

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
                ( progDesc ("Submit a problem to Codeforces. " ++
                  "FILE can be of the form 100A.cpp else PROB or LANG will have to be provided. " ++
                  "If omitted, will attempt to submit the latest file in the current directory"))))

opts = info ( helper <*> version <*> parser ) idm

run :: Args -> IO()
run (Args (Login (LoginOpts handle)))            = initLogin handle
run (Args (Submit (SubmitOpts file cont prob lang ))) = initSubmit file cont prob lang

start :: IO ()
start = execParser opts >>= run
