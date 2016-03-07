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

data DNode = Directory Entry (IO [DNode]) | File Entry Text

fetch :: IO [DNode]
fetch = do
    root <- getServpath >>= \case 
                Nothing -> error "no server path configured in ~/.pgdl"
                Just p -> return p
    let rootUrl = "http://" `T.append` root
    html <- getWebpage rootUrl
    let
        entries = parseDirectoryListing html
        -- | TODO: reuse network connection manager to avoid
        -- TlsExceptionHostPort (HandshakeFailed (Error_Packet_unexpected "Alert [(AlertLevel_Fatal,BadRecordMac)]" " expected: change cipher")) "www.kernel.org" 80
        -- (Maybe reuse network connection manager can avoid this error)
        -- However, pgdl-rewrite will definitely use http instead of simpleHTTP after UI design
        toDNode :: Text -> Entry -> IO DNode
        toDNode url e
            | isDirectory e = return $ Directory e childs
            | otherwise = return $ File e (url `T.append` href e)
            where
            childs :: IO [DNode]
            childs = do
                html' <- getWebpage newUrl
                mapM (toDNode newUrl) $ parseDirectoryListing html'
                where
                newUrl = url `T.append` href e 
    rootNodes <- mapM (toDNode rootUrl) entries
    return rootNodes

