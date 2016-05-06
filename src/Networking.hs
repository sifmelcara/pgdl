{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Networking
where

import Network.HTTP.Conduit
import Data.Text.Encoding
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Ord
import Data.List
import Data.Maybe
import Text.HTML.DirectoryListing.Type
import Text.HTML.DirectoryListing.Parser
import System.FilePath.Posix
import Control.Concurrent

import Cache
import qualified Configure as Conf
import Types
import Local

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
            path = T.unpack url ++ T.unpack rp
            auth = case up of
                    Nothing -> id
                    Just (u, p) -> applyBasicAuth (encodeUtf8 u) (encodeUtf8 p)
    manager <- newManager tlsManagerSettings
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

fetch :: NetworkResource ->
         IO [DNode]
fetch nr = do
    html <- getWebpage nr ""
    lcd <- T.unpack . fromMaybe "" <$> Conf.getLocaldir
    let
        entries = sortBy (flip $ comparing lastModified) $ parseDirectoryListing html
        toDNode :: Text -> Entry -> IO DNode
        toDNode url e
            | isDirectory e = return $ Directory e childs
            | otherwise = do
                downloaded <- isFileDownloaded (decodedName e) lcd
                return $ File e (url `T.append` href e) downloaded
            where
            childs :: IO [DNode]
            childs = do
                html' <- getWebpage nr newUrl
                mapM (toDNode newUrl) . sortBy (flip $ comparing lastModified) $ parseDirectoryListing html'
            newUrl = url `T.append` href e 
    forkIO $ writeCache entries
    mapM (toDNode "") entries
