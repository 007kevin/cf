{-# LANGUAGE OverloadedStrings #-}
module Login (initLogin) where

import           AppError
import           Control.Error
import           Control.Exception
import           Control.Lens ((^.))
import           Control.Monad.Plus (guard)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe
import           CookieSaver
import           Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as Char
import           Metadata (login_url)
import qualified Network.HTTP.Client.Internal as HTTP
import           Network.Wreq
import qualified Network.Wreq.Session as Sess
import           System.IO
import           Text.Pretty.Simple (pPrint)
import           Text.Regex.TDFA

initLogin :: String -> IO()
initLogin handle =  do
  password <- getPassword
  result <- runExceptT $ attemptLogin handle password
  case result of
    Right v          -> pPrint v
    Left FailedLogin -> putStrLn "Login failed"
    Left e           -> putStrLn $ show e

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  password <- withoutEcho getLine
  putChar '\n'
  return password
  where withoutEcho :: IO a -> IO a
        withoutEcho action = do
          old <- hGetEcho stdin
          bracket_ (hSetEcho stdin False) (hSetEcho stdin old) action

attemptLogin :: String -> String -> ExceptT AppError IO ()
attemptLogin handle password = do
  session <- lift Sess.newSession
  csrf_token <- getCsrfToken session login_url
  res <- lift $ Sess.post session login_url [ "handleOrEmail" := (handle::String),
                                              "password"      := (password::String),
                                              "csrf_token"    := (csrf_token::String),
                                              "remember"      := ("no"::String),
                                              "action"        := ("enter"::String) ]
  isLoginSuccess res
  cookies <- (Sess.getSessionCookieJar session) !? AppError "unable to get cookiejar"
  saveCookieJar cookies

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

isLoginSuccess :: Response Char.ByteString -> ExceptT AppError IO ()
isLoginSuccess = (?? FailedLogin).notM.regex.Char.unpack.(^. responseBody)
  where
    regex :: String -> Maybe String
    regex = (=~~ ("\"error for__password\""::String))
    notM :: Maybe String -> Maybe ()
    notM m = case m of
      Just _ -> Nothing
      Nothing -> Just ()
