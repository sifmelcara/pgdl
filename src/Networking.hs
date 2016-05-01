{-# LANGUAGE LambdaCase #-}

module Networking
where

import Network.HTTP.Conduit
import Data.Text.Encoding
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Configure

type NetworkResource = Text -> (Request, Manager) 

-- | generate a network resource.
-- this function should only 
genNetworkResource :: IO Text -> IO NetworkResource
genNetworkResource passwordAsker = do
    manager <- newManager tlsManagerSettings
    req <- getUsername >>= \case
            Nothing -> parseUrl (T.unpack url)
            Just name -> getPassword >>= \case
                Nothing -> error "no password."
                Just pw -> applyBasicAuth (encodeUtf8 name) (encodeUtf8 pw) <$> parseUrl (T.unpack url)
    return $ \t -> (genReq t, manager)

getWebpage :: Text -> IO Text
getWebpage url = do
    user <- getUsername
    pass <- getPassword
    req <- case (user, pass) of
            (Just u, Just p) -> applyBasicAuth bu bp <$> parseUrl (T.unpack url)
                where
                [bu, bp] = map encodeUtf8 [u, p]
            (Just u, Nothing) -> 
            _ -> parseUrl $ T.unpack url
    manager <- newManager tlsManagerSettings
    response <- httpLbs req manager
    let body = responseBody response
    case decodeUtf8' . B.concat . BL.toChunks $ body of
        Left unicodeException -> error . show $ unicodeException
        Right t -> return t

