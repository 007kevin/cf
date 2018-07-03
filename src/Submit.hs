{-# LANGUAGE OverloadedStrings #-}
module Submit (initSubmit) where

import           AppError
import           Control.Exception (SomeException)
import           System.FilePath.Posix (takeExtension, takeBaseName)
import           System.Directory (makeAbsolute)
import           Control.Error.Util ((??), note, handleExceptT)
import           Control.Lens ((^.))
import           Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import           Control.Monad.Trans.Class (lift)
import           System.Directory (XdgDirectory(XdgConfig), getXdgDirectory)
import           Text.Pretty.Simple (pPrint)
import           ProgressIndicator
import           DataSaver
import           Metadata (submit_url, submit_url1, language)
import           Network.HTTP.Client(defaultManagerSettings)
import           Network.HTTP.Client.Internal (Cookie)
import           Network.Wreq
import qualified Data.ByteString.Lazy.Char8 as Char
import qualified Network.Wreq.Session as Sess
import           Text.Regex.TDFA

initSubmit :: String -> Maybe String -> Maybe String -> IO ()
initSubmit file prob lang = do 
  result <- runExceptT $ start file prob lang
  case result of
    Right v -> pPrint v
    Left e -> pPrint e

-- start :: String -> Maybe String -> Maybe String -> ExceptT AppError IO ()
start file prob lang = do
  (c,p,l) <- (infer file prob lang) ?? AppError ("unable to infer information from " ++ file)
  attemptSubmit file c p l

attemptSubmit :: String -> String -> String -> String -> ExceptT AppError IO ()
attemptSubmit file contest prob lang = do
  cookieJar <- loadCookieJar
  lift $ putStrLn $ "Submitting " ++ file ++ " to contest " ++ contest ++ " for problem " ++ prob
  session <- lift $ Sess.newSessionControl (Just cookieJar) defaultManagerSettings
  csrf_token <- getCsrfToken session (submit_url contest)
  source <- getSource file
  res <- lift $ Sess.post session (submit_url1 contest csrf_token )
         [ "csrf_token" := (csrf_token::String),
           "action":= ("submitSolutionFormSubmitted"::String),
           "submittedProblemIndex" := (prob::String),
           "programTypeId" := (language lang),
           "source" := (source::String) ]
  pPrint $ res ^. responseStatus.statusCode
  return ()

infer :: String -> Maybe String -> Maybe String -> Maybe (String, String, String)
infer file (Just p) (Just l) = return (file,p,l)
infer file Nothing Nothing = do
  c <- cont file
  p <- prob file
  l <- lang file
  return (c, p, l)
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

      
