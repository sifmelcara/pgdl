{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Fetcher
where

import Data.Text (Text)
import qualified Data.Text as T
import Text.HTML.DirectoryListing.Type
import Text.HTML.DirectoryListing.Parser
import Data.Ord
import Data.List
import Data.Maybe
import Control.Concurrent

import Cache
import qualified Configure as Conf
import Local
import Types
import Networking

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
                html' <- getWebpage nr (href e)
                mapM (toDNode newUrl) . sortBy (flip $ comparing lastModified) $ parseDirectoryListing html'
            newUrl = url `T.append` href e 
    forkIO $ writeCache entries
    mapM (toDNode "") entries

