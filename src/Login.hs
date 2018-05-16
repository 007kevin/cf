{-# LANGUAGE OverloadedStrings #-}

module Login (initLogin) where

import Metadata
import Network.Wreq
import Control.Lens
import Text.Regex.Posix
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Network.Wreq.Session as S

initLogin :: String -> IO()
initLogin handle =  putStrLn handle

-- extract the csrf token from the response body
-- getToken :: String -> String
-- getToken body = result!!0!!0
--   where
--     result :: [[String]]
--     result = body =~ "name=\'csrf_token\' +value=([^\']+)"

login :: IO ()
login = do
  session <- S.newSession
  S.get session (cfURL++"/enter")
  res <- S.post session cfURL [
    "action" := ("enter"::String),
      "handle" := ("testuser"::String),
      "password" := ("pass"::String),
      "remember" := ("yes"::String)
    ]
  print $ res ^. responseBody
  
regex :: String -> [[String]]
regex = (flip (=~)) ("name='csrf_token' +value='([^\']+)'"::String)
test = (flip (^.)) responseBody

testfn = fmap (regex.(C.unpack).test) (get (cfURL++"/enter"))




  
  
  
  
