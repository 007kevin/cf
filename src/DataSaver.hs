-- Save and read data/cookies for subsequent requests
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module DataSaver where

import GHC.Generics
import AppError
import Control.Error
import Control.Error.Util ((??))
import Control.Exception (SomeException)
import Data.List (find)
import System.FilePath.Posix (takeExtension, takeBaseName)
import qualified Network.HTTP.Client.Internal as HTTP
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy.Char8 as Char
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text.Encoding
import Control.Monad.Trans.Except (ExceptT)
import           System.Directory (XdgDirectory( XdgConfig ),
                                   getXdgDirectory,
                                   createDirectoryIfMissing,
                                   doesPathExist)

instance ToJSON HTTP.Cookie where
  toJSON (HTTP.Cookie
          {HTTP.cookie_name             , HTTP.cookie_value,
           HTTP.cookie_expiry_time      , HTTP.cookie_domain,
           HTTP.cookie_path             , HTTP.cookie_creation_time,
           HTTP.cookie_last_access_time , HTTP.cookie_persistent,
           HTTP.cookie_host_only        , HTTP.cookie_secure_only,
           HTTP.cookie_http_only }
         ) = object ["cookie_name"             .= decodeUtf8 cookie_name,
                     "cookie_value"            .= decodeUtf8 cookie_value,
                     "cookie_expiry_time"      .= cookie_expiry_time,
                     "cookie_domain"           .= decodeUtf8 cookie_domain,
                     "cookie_path"             .= decodeUtf8 cookie_path,
                     "cookie_creation_time"    .= cookie_creation_time,
                     "cookie_last_access_time" .= cookie_last_access_time,
                     "cookie_persistent"       .= cookie_persistent,
                     "cookie_host_only"        .= cookie_host_only,
                     "cookie_secure_only"      .= cookie_secure_only,
                     "cookie_http_only"        .= cookie_http_only]

instance FromJSON HTTP.Cookie where
  parseJSON = withObject "Cookie" $ \v ->
    HTTP.Cookie
    <$> (fmap encodeUtf8 (v .: "cookie_name"))
    <*> (fmap encodeUtf8 (v .: "cookie_value"))
    <*> v .: "cookie_expiry_time"
    <*> (fmap encodeUtf8 (v .: "cookie_domain"))
    <*> (fmap encodeUtf8 (v .: "cookie_path"))
    <*> v  .: "cookie_creation_time" 
    <*> v .: "cookie_last_access_time" 
    <*> v .: "cookie_persistent" 
    <*> v .: "cookie_host_only" 
    <*> v .: "cookie_secure_only" 
    <*> v .: "cookie_http_only"

instance ToJSON Language
instance FromJSON Language
instance ToJSON UserConfig
instance FromJSON UserConfig

data Language = Language
  { extension :: String,
    code :: String,
    command :: String }
  deriving (Generic, Show)

data UserConfig = UserConfig
  { handle :: String,
    languages :: [Language] }
  deriving (Generic, Show)

defaultUserConfig = UserConfig
  { handle = "",
    languages = [ Language { extension = "c",
                             code = "10",
                             command = "" },
                  Language { extension = "d",
                             code = "28",
                             command = "" },
                  Language { extension = "pl",
                             code = "13",
                             command = "" },
                  Language { extension = "php",
                             code = "6",
                             command = "" },
                  Language { extension = "ml",
                             code = "19",
                             command = "" },
                  Language { extension = "cpp",
                             code = "54", -- GNU G++17 by default
                             command = "" },
                  Language { extension = "rb",
                             code = "8",
                             command = "" },
                  Language { extension = "scala",
                             code = "20",
                             command = "" },
                  Language { extension = "hs",
                             code = "12",
                             command = "" },
                  Language { extension = "pas",
                             code = "4",
                             command = "" },
                  Language { extension = "cs",
                             code = "29",
                             command = "" },
                  Language { extension = "js",
                             code = "34",
                             command = "" },
                  Language { extension = "java",
                             code = "36",
                             command = "" },
                  Language { extension = "go",
                             code = "32",
                             command = "" },
                  Language { extension = "tcl",
                             code = "14",
                             command = "" },
                  Language { extension = "py",
                             code = "31",
                             command = "" } ] }

language :: String -> UserConfig -> ExceptT AppError IO String
language ext userConfig = go ?? EConfig ("Unable to match extension " ++ ext)
  where go = fmap code (find (\x -> ext == extension x ) (languages userConfig))

cookieSaveName = "user.cookies"
configSaveName = "config.json"

-- Returns the application file path for storing settings. In Unix systems
-- this will be $HOME/.config/cf/<fname>
configFilePath :: ExceptT AppError IO FilePath
configFilePath = do
  fdir <- lift $ getXdgDirectory XdgConfig "cf/"
  handleExceptT handler $ createDirectoryIfMissing True fdir
  return $ fdir
  where handler :: SomeException -> AppError
        handler e = EWriteFile (show e) 
  
saveCookieJar :: HTTP.CookieJar -> ExceptT AppError IO ()
saveCookieJar cookies = do
  fpath <- fmap (++cookieSaveName) configFilePath
  lift $ putStrLn ("Saving cookies to " ++ fpath)
  handleExceptT handler . writeFile fpath $ (Char.unpack.encode.HTTP.expose) cookies
  where handler :: SomeException -> AppError
        handler e = EWriteFile (show e)

loadCookieJar :: ExceptT AppError IO HTTP.CookieJar
loadCookieJar = do
  fpath <- fmap (++cookieSaveName) configFilePath
  -- lift $ putStrLn ("Loading cookies from " ++ fpath)  
  content <- handleExceptT handler (Char.readFile fpath)
  cookies <- (decode content) ?? EParse "unable to decode cookies"
  return $ HTTP.createCookieJar cookies
  where handler :: SomeException -> AppError
        handler e = EReadFile (show e)

-- Returns the defaultUserConfig unless UserConfig already exists
loadUserConfig :: ExceptT AppError IO UserConfig
loadUserConfig = do
  fpath <- fmap (++configSaveName) configFilePath
  exist <- handleExceptT handler $ doesPathExist fpath
  case exist of
    False -> return defaultUserConfig
    True -> do
      content <- handleExceptT handler (Char.readFile fpath)
      userConfig <- (decode content) ?? EParse "unable to load user config"
      return userConfig
  where handler :: SomeException -> AppError
        handler e = EReadFile (show e)

saveUserConfig :: UserConfig -> ExceptT AppError IO ()
saveUserConfig userconfig = do
  fpath <- fmap (++configSaveName) configFilePath
  lift $ putStrLn ("Saving configuration to " ++ fpath)
  handleExceptT handler . writeFile fpath $ (Char.unpack.encodePretty) userconfig
  where handler :: SomeException -> AppError
        handler e = EWriteFile (show e)
