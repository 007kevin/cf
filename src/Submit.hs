{-# LANGUAGE OverloadedStrings #-}
module Submit (initSubmit) where

import           AppError
import           Control.Exception (SomeException, throw)
import           Control.Concurrent (threadDelay)
import           System.FilePath.Posix (takeExtension, takeBaseName)
import           System.IO
import           System.Directory (makeAbsolute)
import           Control.Error.Util ((??), note, handleExceptT)
import           Control.Lens ((^.), (^?))
import           Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE)
import           Control.Monad.Trans.Class (lift)
import           Data.Aeson.Lens (key, _String, nth)
import           Data.List (sort)
import           System.Directory (XdgDirectory(XdgConfig),
                                   getXdgDirectory,
                                   getCurrentDirectory,
                                   getDirectoryContents,
                                   exeExtension,
                                   getAccessTime)
import           Text.Pretty.Simple (pPrint)
import           ProgressIndicator
import           DataSaver
import           Metadata (submit_url, submit_url1, submission_url)
import           Network.HTTP.Client (defaultManagerSettings)
import           Network.HTTP.Client.Internal (Cookie)
import           Network.Wreq
import qualified Data.ByteString.Lazy.Char8 as Char
import qualified Data.Text as Text
import qualified Network.Wreq.Session as Sess
import           Text.Regex.TDFA

initSubmit :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> IO ()
initSubmit file cont prob lang = do
  result <- runExceptT $ start file cont prob lang
  case result of
    Right _  -> return ()
    Left e -> pPrint e

start :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> ExceptT AppError IO ()
start file cont prob lang = do
  (f,c,p,l) <- extractInfo file cont prob lang
  attemptSubmit f c p l
  
attemptSubmit :: String -> String -> String -> String -> ExceptT AppError IO ()
attemptSubmit file contest prob lang = do
  cookieJar <- loadCookieJar
  session <- lift $ Sess.newSessionControl (Just cookieJar) defaultManagerSettings
  csrf_token <- getCsrfToken session (submit_url contest)
  source <- getSource file
  userConfig <- loadUserConfig
  langId <- language lang userConfig
  res <- lift $ Sess.post session (submit_url1 contest csrf_token )
         [ "csrf_token"            := (csrf_token::String),
           "action"                := ("submitSolutionFormSubmitted"::String),
           "submittedProblemIndex" := (prob::String),
           "programTypeId"         := (langId::String),
           "source"                := (source::String) ]
  lift $ putStrLn (show (res ^. responseStatus . statusCode))
  status <- lift $ submissionStatus (handle userConfig)
  lift $ putStrLn status
  return ()

extractInfo :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> ExceptT AppError IO (String, String, String, String)
extractInfo Nothing Nothing Nothing Nothing = mostRecentFile
extractInfo file cont prob lang = (infer file cont prob lang) ?? AppError ("unable to infer information from " ++ (show' file))
  where show' m = case m of
                    Just m -> m
                    Nothing -> "unknown"

mostRecentFile :: ExceptT AppError IO (String, String, String, String)
mostRecentFile = do
  allowed <- fmap (map extension.languages) loadUserConfig
  -- get files in the current directory with file extensions found in the user config
  files <- lift $ fmap
           (filter ((`elem` allowed).rmDot.takeExtension))
           (getDirectoryContents =<< getCurrentDirectory)
  -- among the files, find the latest         
  latest <- lift $ fmap (snd.head.reverse.sort.(`zip` files)) (sequence (map getAccessTime files))
  extractInfo (Just latest ) Nothing Nothing Nothing
  where rmDot e = if null e || head e /= '.' then e else tail e
  
infer :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe (String, String, String, String)
infer (Just file) (Just cont) (Just prob) (Just lang) = return (file, cont, prob,lang)
infer (Just file) Nothing Nothing Nothing = do
  c <- cont file
  p <- prob file
  l <- lang file
  return (file, c, p, l)
  where
    cont :: String -> Maybe String
    cont f = do
      baseName <- case takeBaseName f of "" -> Nothing; b -> return b
      baseName =~~ ("[0-9]+"::String)
    prob :: String -> Maybe String
    prob f = do
      baseName <- case takeBaseName f of "" -> Nothing; b -> return b
      baseName =~~ ("[A-Za-z]+$"::String)
    lang :: String -> Maybe String
    lang f = case takeExtension f of
      "" -> Nothing
      e -> Just (tail e) -- remove the dot
-- get the latest file with file ex      

getCsrfToken :: Sess.Session -> String -> ExceptT AppError IO String
getCsrfToken session url =
  ExceptT $ fmap (note'.extractCsrf) (Sess.get session url)
  where note' = note $ EParse "could not extract csrf token"

extractCsrf :: Response Char.ByteString -> Maybe String
extractCsrf res = do
  match <- regex (Char.unpack (res ^. responseBody))
  return $ last (head match)
  where
    regex :: String -> Maybe [[String]]
    regex = (=~~ ("name='csrf_token' +value='([^\']+)'"::String))

getSource :: FilePath -> ExceptT AppError IO String
getSource fpath = do
  apath <- lift $ makeAbsolute fpath
  source <- handleExceptT handler (readFile apath)
  return source
  where handler :: SomeException -> AppError
        handler e = EReadFile (show e)

checkSubmission :: Sess.Session -> String -> IO (Maybe String)
checkSubmission session handle = do
  res <- Sess.get session (submission_url handle)
  return $ do
    status <- fmap Text.unpack (res ^? responseBody . key "status" . _String)
    verdict <- fmap Text.unpack (res ^? responseBody . key "result" . nth 0 . key "verdict" .  _String)
    return verdict

submissionStatus :: String -> IO String
submissionStatus handle = do
  Sess.newAPISession >>= (\s -> go s 0)
  where
    go session cnt = do
      threadDelay 150000
      res <- checkSubmission session handle
      case res of
        Just "TESTING" -> hPutStr stderr ("\r\ESC[KTesting" ++ (replicate (cnt `rem` 4) '.'))  >> go session (cnt+1)
        Just verdict -> hPutStr stderr "\r\ESC[K" >> return verdict
        Nothing -> go session (cnt+1)
