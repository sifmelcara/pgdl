{-# LANGUAGE OverloadedStrings #-}

module FetchHtml where
  
import Getconfig

import Network.URI
import Data.Text.Encoding
import Data.Maybe
import System.FilePath
import Control.Applicative
import qualified Data.Text as T
import Network.HTTP.Conduit
import Data.ByteString.Lazy
--import bytestring?

fetchHtml :: IO T.Text
fetchHtml = getTextContent =<< genURI

fetchFld :: T.Text -> IO T.Text
fetchFld fnt = do
    uri <- (</> fn) <$> genURI
    getTextContent uri
    where fn = T.unpack fnt

genURI :: IO String
genURI = do
    usrn <- getUsername
    usrp <- getPassword
    servp <- getServpath
    return $ "http://" ++ usrn ++ ":" ++ usrp ++ "@" ++ servp

getTextContent :: String -> IO T.Text
getTextContent u = do
    bs <- simpleHttp u
    let t = decodeUtf8 . toStrict $ bs
    return t
    

