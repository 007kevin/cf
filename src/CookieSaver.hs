-- Save and read cookies for subsequent requests
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module CookieSaver where

import AppError
import Control.Error
import Control.Error.Util ((??))
import Control.Exception
import System.FilePath.Posix (takeExtension, takeBaseName)
import qualified Network.HTTP.Client.Internal as HTTP
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy.Char8 as Char
import Data.Aeson
import Data.Text.Encoding
import Control.Monad.Trans.Except (ExceptT)
import           System.Directory (XdgDirectory( XdgConfig ),
                                   getXdgDirectory,
                                   createDirectoryIfMissing)


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
  fpath <- fmap (++"user.cookies") configFilePath
  lift $ putStrLn ("Saving cookies to " ++ fpath)
  handleExceptT handler . writeFile fpath $ (Char.unpack.encode.HTTP.expose) cookies
  where handler :: SomeException -> AppError
        handler e = EWriteFile (show e)

loadCookieJar :: ExceptT AppError IO HTTP.CookieJar
loadCookieJar = do
  fpath <- fmap (++"user.cookies") configFilePath
  content <- handleExceptT handler (Char.readFile fpath)
  cookies <- (decode content) ?? EParse "unable to decode cookies"
  return $ cookies
  where handler :: SomeException -> AppError
        handler e = EReadFile (show e)
  
  
  
