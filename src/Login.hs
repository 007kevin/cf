{-# LANGUAGE OverloadedStrings #-}
module Login (initLogin) where

import System.IO
import Control.Exception
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Plus (guard)
import Text.Pretty.Simple (pPrint)
import qualified Metadata as M
import Network.Wreq
import Control.Lens
import Text.Regex.TDFA
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Network.Wreq.Session as S

login_url :: String
login_url = M.url ++ "/enter"
  
initLogin :: String -> IO()
initLogin handle =  do
  password <- getPassword
  session <- S.newSession
  maybe <- runMaybeT $ attemptLogin session handle password
  case maybe of
    Just v -> pPrint v
    Nothing -> putStrLn "Login failed"

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

extractCsrf :: Response C.ByteString -> Maybe String
extractCsrf res = do
  match <- regex (C.unpack (res ^. responseBody))
  return $ last (head match)
  where
    regex :: String -> Maybe [[String]]
    regex = (=~~ ("name='csrf_token' +value='([^\']+)'"::String))

getCsrfToken :: S.Session -> String -> MaybeT IO String
getCsrfToken session url = MaybeT $ fmap extractCsrf (S.get session url)

isLoginSuccess :: Response C.ByteString -> Bool
isLoginSuccess res =
  case regex (C.unpack (res ^. responseBody)) of
    Just _ -> False
    Nothing -> True
  where
    regex :: String -> Maybe [[String]]
    regex = (=~~ ("\"error for__password\""::String))

-- attemptLogin :: S.Session -> String -> String -> MaybeT IO a
attemptLogin session handle password = do
  csrf_token <- getCsrfToken session login_url
  res <- lift $ S.post session login_url [ "handleOrEmail" := (handle::String),
                                           "password"      := (password::String),
                                           "csrf_token"    := (csrf_token::String),
                                           "remember"      := ("no"::String),
                                           "action"        := ("enter"::String) ]
  guard $ isLoginSuccess res
  return $ res ^. responseCookieJar
