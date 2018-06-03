{-# LANGUAGE OverloadedStrings #-}
module Login (initLogin) where

import System.IO
import Control.Exception
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Plus (guard)
import Text.Pretty.Simple (pPrint)
import qualified Metadata as M
import CookieSaver
import Network.Wreq
import Control.Lens
import Text.Regex.TDFA
import qualified Data.ByteString.Lazy.Char8 as Char
import qualified Network.Wreq.Session as Sess
import qualified Network.HTTP.Client.Internal as HTTP
import Data.Aeson (encode, decode)

login_url :: String
login_url = M.url ++ "/enter"
  
initLogin :: String -> IO()
initLogin handle =  do
  password <- getPassword
  maybe <- runMaybeT $ attemptLogin handle password
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

extractCsrf :: Response Char.ByteString -> Maybe String
extractCsrf res = do
  match <- regex (Char.unpack (res ^. responseBody))
  return $ last (head match)
  where
    regex :: String -> Maybe [[String]]
    regex = (=~~ ("name='csrf_token' +value='([^\']+)'"::String))

getCsrfToken :: Sess.Session -> String -> MaybeT IO String
getCsrfToken session url = MaybeT $ fmap extractCsrf (Sess.get session url)

isLoginSuccess :: Response Char.ByteString -> Bool
isLoginSuccess res =
  case regex (Char.unpack (res ^. responseBody)) of
    Just _ -> False
    Nothing -> True
  where
    regex :: String -> Maybe [[String]]
    regex = (=~~ ("\"error for__password\""::String))

-- saveCookieJar ::  CookieJar -> MaybeT IO ()

-- attemptLogin :: Sess.Session -> String -> String -> MaybeT IO a
attemptLogin handle password = do
  session <- lift Sess.newSession
  csrf_token <- getCsrfToken session login_url
  res <- lift $ Sess.post session login_url [ "handleOrEmail" := (handle::String),
                                              "password"      := (password::String),
                                              "csrf_token"    := (csrf_token::String),
                                              "remember"      := ("no"::String),
                                              "action"        := ("enter"::String) ]
  guard $ isLoginSuccess res
  cookies <- MaybeT $ Sess.getSessionCookieJar session
  return $ (encode.HTTP.expose) cookies
  
-- saveCookies :: CookieJar -> MaybeT IO ()
-- saveCookies :: liftHTTP.destroyCookieJarJar CookieJar


-- printCookies :: CookieJar -> IO ()
-- printCookies CookieJar cookies = pPrint cookies
