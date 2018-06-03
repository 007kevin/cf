-- Save and read cookies for subsequent requests
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module CookieSaver where

import qualified Network.HTTP.Client.Internal as HTTP
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Plus (guard)
import Data.Aeson
import Data.Text.Encoding

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
    
    -- {HTTP.cookie_name             = (v .: "cookie_name"),
    --  HTTP.cookie_value            = (v .: "cookie_value"),
    --  HTTP.cookie_expiry_time      = (v .: "cookie_expiry_time"),
    --  HTTP.cookie_domain           = (v .: "cookie_domain" ),
    --  HTTP.cookie_path             = (v .: "cookie_path" ),
    --  HTTP.cookie_creation_time    = (v  .: "cookie_creation_time" ),
    --  HTTP.cookie_last_access_time = (v .: "cookie_last_access_time" ),
    --  HTTP.cookie_persistent       = (v .: "cookie_persistent" ),
    --  HTTP.cookie_host_only        = (v .: "cookie_host_only" ),
    --  HTTP.cookie_secure_only      = (v .: "cookie_secure_only" ),
    --  HTTP.cookie_http_only        = (v .: "cookie_http_only" )



-- saveCookies CookieJar -> MaybeT IO ()
-- saveCookies cookieJar -> HTTP.expose cookieJar
