{-# LANGUAGE OverloadedStrings #-}

module FetchHtml where
  
import Getconfig

import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString as B
import Data.Text.Encoding
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Maybe
import Data.Text.ICU.Convert

genReq :: IO Request
genReq = do
    username <- getUsername
    password <- getPassword
    servpath <- getServpath
    return $ applyBasicAuth (tob username) (tob password) $ fromJust $ parseUrl $ "http://" ++ (T.unpack $ servpath)
    where tob = encodeUtf8

fetchHtml :: IO T.Text
fetchHtml = genReq >>=
              (\rq -> runResourceT $ do
                mgr <- liftIO $ newManager conduitManagerSettings
                res <- httpLbs rq mgr
                conv <- liftIO $ open "utf-8" Nothing
                liftIO . return . toUnicode conv . LC.toStrict $ responseBody res)


