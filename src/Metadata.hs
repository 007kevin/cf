module Metadata where

appVersion :: String
appVersion = "0.0.0"
  
-- Codeforces URL
url :: String
url = "http://codeforces.com"

login_url :: String
login_url = url++"/enter"

submit_url :: String -> String
submit_url contest = url++"/contest/"++contest++"/submit"

submit_url1 :: String -> String -> String
submit_url1 contest csrf_token = url++"/contest/"++contest++"/submit?csrf_token="++csrf_token

submission_url :: String -> String
submission_url handle = url++"/api/user.status?handle="++handle++"&from=1&count=1"
