{-# LANGUAGE OverloadedStrings #-}
module Login (initLogin) where

import Text.Pretty.Simple (pPrint)
import qualified Metadata as M
import Network.Wreq
import Control.Lens
import Text.Regex.TDFA
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Network.Wreq.Session as S

initLogin :: String -> IO()
initLogin handle =  putStrLn handle

-- TODO: Refactor ugly code with maybe monad error handling
extractCsrf :: Response C.ByteString -> String
extractCsrf = head.tail.head.regex.C.unpack.(^. responseBody)
  where
    regex :: String -> [[String]]
    regex = (=~ ("name='csrf_token' +value='([^\']+)'"::String))

login :: IO ()
login = let loginURL = M.url ++ "/enter" in
  do session <- S.newSession
     csrf_token <- extractCsrf <$> S.get session loginURL
     res <- S.post session loginURL [ "action"        := ("enter"::String),
                                      "handleOrEmail" := (""::String),
                                      "password"      := (""::String),
                                      "remember"      := ("yes"::String),
                                      "csrf_token"         := (csrf_token::String) ]
     print $ testLogin (C.unpack (res ^. responseBody))
     cookie <- S.getSessionCookieJar session
     case cookie of
       Just jar -> pPrint jar
       Nothing -> print "Cookie not found"

testLogin :: String -> String
testLogin = (=~ ("\"error for__password\""::String))



  
  
  
  
