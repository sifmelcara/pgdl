{-# LANGUAGE LambdaCase #-}

module Networking
where

import Network.HTTP.Conduit
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Configure

getWebpage :: T.Text -> IO T.Text
getWebpage url = do
    user <- getUsername
    pass <- getPassword
    req <- case (user, pass) of
            (Just u, Just p) -> applyBasicAuth bu bp <$> parseUrl (T.unpack url)
                where
                [bu, bp] = map encodeUtf8 [u, p]
            _ -> parseUrl $ T.unpack url
    manager <- newManager tlsManagerSettings
    response <- httpLbs req manager
    let body = responseBody response
    case decodeUtf8' . B.concat . BL.toChunks $ body of
        Left unicodeException -> error . show $ unicodeException
        Right t -> return t

