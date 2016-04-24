{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Fetcher
where

import Configure
import Networking
import Text.HTML.DirectoryListing.Type
import Text.HTML.DirectoryListing.Parser
import qualified Data.Text as T
import Data.Text (Text)
import Cache
import Types
import Control.Concurrent
import Data.Ord
import Data.List
import Data.Maybe
import Local

fetch :: Text -> -- ^ root url
         IO [DNode]
fetch rootUrl = do
    html <- getWebpage rootUrl
    lcd <- T.unpack . fromMaybe "" <$> getLocaldir
    let
        entries = sortBy (flip $ comparing lastModified) $ parseDirectoryListing html
        -- | TODO: reuse network connection manager to avoid
        -- TlsExceptionHostPort (HandshakeFailed (Error_Packet_unexpected "Alert [(AlertLevel_Fatal,BadRecordMac)]" " expected: change cipher")) "www.kernel.org" 80
        -- (Maybe reuse network connection manager can avoid this error)
        -- However, pgdl-rewrite will definitely use http instead of simpleHTTP after UI design
        toDNode :: Text -> Entry -> IO DNode
        toDNode url e
            | isDirectory e = return $ Directory e childs
            | otherwise = do
                downloaded <- isFileDownloaded (decodedName e) lcd
                return $ File e (url `T.append` href e) downloaded
            where
            childs :: IO [DNode]
            childs = do
                html' <- getWebpage newUrl
                mapM (toDNode newUrl) . sortBy (flip $ comparing lastModified) $ parseDirectoryListing html'
                where
                newUrl = url `T.append` href e 
    forkIO $ writeCache entries
    rootNodes <- mapM (toDNode rootUrl) entries
    return rootNodes

