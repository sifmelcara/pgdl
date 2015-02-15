{-# LANGUAGE OverloadedStrings #-}

module FetchHtml where
  
import Getconfig

import Network.HTTP
import Network.URI
import Data.Text.Encoding
import Data.Maybe
import qualified Data.Text as T

fetchHtml :: IO T.Text
fetchHtml = do
    usrn <- getUsername
    usrp <- getPassword
    servp <- getServpath
    let uri = "http://" ++ usrn ++ ":" ++ usrp ++ "@" ++ servp
    res <- simpleHTTP . mkRequest GET . fromJust . parseURI $ uri
    cnt <- getResponseBody res
    return $ decodeUtf8 cnt

