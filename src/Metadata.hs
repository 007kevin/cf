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

-- Languages mapping
language :: String -> String
language "c"     = "10"
language "d"     = "28"
language "pl"    = "13"
language "php"   = "6"
language "ml"    = "19"
language "cpp"   = "1"
language "rb"    = "8"
language "scala" = "20"
language "hs"    = "12"
language "pas"   = "4"
language "cs"    = "29"
language "js"    = "34"
language "java"  = "36"
language "go"    = "32"
language "tcl"   = "14"
language "py"    = "31"


