{-# LANGUAGE OverloadedStrings #-}

module FetchHtml where
  
import Getconfig

import Control.Monad.IO.Class 
import Control.Monad.Trans.Resource 
import Data.Text.Encoding
import Data.Maybe
import Network.HTTP.Conduit

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString as B

genReq :: IO Request
genReq = do
    username <- getUsername
    password <- getPassword
    servpath <- getServpath
    return $ applyBasicAuth (tob username) (tob password) . fromJust . parseUrl $ "http://" ++ T.unpack servpath
    where tob = encodeUtf8

fetchHtml :: IO T.Text
fetchHtml = do
    rq <- genReq
    runResourceT $ do
        mgr <- liftIO $ newManager conduitManagerSettings
        res <- httpLbs rq mgr
        -- conv <- liftIO $ open "utf-8" Nothing
        -- liftIO . return . toUnicode conv . LC.toStrict $ responseBody res
        return . decodeUtf8 . LC.toStrict $ responseBody res


