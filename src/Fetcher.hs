{-# LANGUAGE OverloadedStrings #-}
module Fetcher
where

import Configure
import Networking
import Text.HTML.DirectoryListing.Type
import Text.HTML.DirectoryListing.Parser
import qualified Data.Text as T
import Data.Text (Text)

data DNode = Directory Entry (IO [DNode]) | File Entry

fetch :: IO [DNode]
fetch = do
    root <- getServpath
    let rootUrl = "http://" `T.append` root
    html <- getWebpage rootUrl
    let
        entries = parseDirectoryListing html
        toDNode :: Text -> Entry -> IO DNode
        toDNode url e
            | isDirectory e = return $ Directory e childs
            | otherwise = return $ File e
            where
            childs :: IO [DNode]
            childs = do
                html' <- getWebpage newUrl
                mapM (toDNode newUrl) $ parseDirectoryListing html'
                where
                newUrl = url `T.append` href e 
    rootNodes <- mapM (toDNode rootUrl) entries
    return rootNodes

