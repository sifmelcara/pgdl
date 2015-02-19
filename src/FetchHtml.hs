{-# LANGUAGE OverloadedStrings #-}

module FetchHtml where
  
import Getconfig

import Network.HTTP
import Network.URI
import Data.Text.Encoding
import Data.Maybe
import System.FilePath
import Control.Applicative
import qualified Data.Text as T

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
    res <- simpleHTTP . mkRequest GET . fromJust . parseURI $ u
    cnt <- getResponseBody res
    return $ decodeUtf8 cnt
    

