{-# LANGUAGE LambdaCase #-}

module Networking
where

import Network.HTTP.Conduit
import Data.Text.Encoding
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.FilePath.Posix

import Configure

type NetworkResource = Text -> (Request, Manager) 
--                    ^ relative path

genNetworkResource :: Text -> -- ^ webpage url (root)
                      Maybe (Text, Text) -> -- ^ (username, password)
                      IO NetworkResource
genNetworkResource url up = do
    let genReq :: Text -> Request
        genReq rp = case parseUrl path of
                        Nothing -> error $ "invalid url: " ++ path
                        Just r -> auth r
            where
            path = (T.unpack url) ++ (T.unpack rp)
            auth = case up of
                    Nothing -> id
                    Just (u, p) -> applyBasicAuth (encodeUtf8 u) (encodeUtf8 p)
    manager <- (newManager tlsManagerSettings :: IO Manager)
    return $ \t -> (genReq t, manager)
    

getWebpage :: NetworkResource -> 
              Text -> -- relative path
              IO Text
getWebpage nr rp = do
    let (req, mgr) = nr rp
    response <- httpLbs req mgr
    let body = responseBody response
    case decodeUtf8' . B.concat . BL.toChunks $ body of
        Left unicodeException -> error . show $ unicodeException
        Right t -> return t

