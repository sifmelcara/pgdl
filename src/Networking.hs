{-# LANGUAGE LambdaCase #-}
module Networking
where

import Configure

import Network.HTTP.Conduit
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Debug.Trace
getWebpage :: T.Text -> IO T.Text
getWebpage url = do
    traceShow url $ return ()
    res <- simpleHttp (T.unpack url)
    case decodeUtf8' . B.concat . BL.toChunks $ res of
        Left unicodeException -> error . show $ unicodeException
        Right t -> return t



