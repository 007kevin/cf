{-# LANGUAGE OverloadedStrings #-}
module Login (initLogin) where

import Metadata
import Network.Wreq
import Control.Lens
import qualified Network.Wreq.Session as S

initLogin :: String -> IO()
initLogin handle =  putStrLn handle

login :: IO ()
login = do
  session <- S.newSession
  S.get session cfURL
  res <- S.post session cfURL [
    "action" := ("enter"::String),
      "handle" := ("testuser"::String),
      "password" := ("pass"::String),
      "remember" := ("yes"::String)
    ]
  print $ res ^. responseCookieJar



