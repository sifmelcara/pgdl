{-# LANGUAGE OverloadedStrings #-}

module FetchHtml where
  
import Getconfig

import Control.Monad.IO.Class 
import Control.Monad.Trans.Resource 
import Data.Text.Encoding
import Data.Maybe
import Network.HTTP.Conduit

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LC

genReq :: IO Request
genReq = do
    username <- getUsername
    password <- getPassword
    servpath <- getServpath
    case username of
        "" -> return $ fromJust . parseUrl $ "http://" ++ T.unpack servpath
        _  -> return $ applyBasicAuth (tob username) (tob password) . fromJust . parseUrl $ "http://" ++ T.unpack servpath
    where tob = encodeUtf8

fetchHtml :: IO T.Text
fetchHtml = do
    rq <- genReq
    runResourceT $ do
        mgr <- liftIO $ newManager conduitManagerSettings
        res <- httpLbs rq mgr
        return . decodeUtf8 . LC.toStrict $ responseBody res


