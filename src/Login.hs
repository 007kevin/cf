{-# LANGUAGE OverloadedStrings #-}
module Login (initLogin) where

import Metadata
import Network.Wreq
import qualified Network.Wreq.Session as S
import Control.Lens

initLogin :: String -> IO()
initLogin handle =  putStrLn handle

login :: IO ()
login = do
  session <- S.newSession
  S.get session cfURL
  res <- S.post session cfURL [
    "action" := "enter",
      "handle" := "testuser",
      "password" := "pass",
      "remember" := "yes"
    ]
  print $ res ^. responseBody



