{-# LANGUAGE OverloadedStrings #-}
module Fetcher
where

import Configure
import Networking
import Text.HTML.DirectoryListing.Type
import Text.HTML.DirectoryListing.Parser
import qualified Data.Text as T

fetchRoot :: IO [Entry]
fetchRoot = do
    root <- getServpath
    html <- getWebpage $ "http://" `T.append` root
    return $ parseDirectoryListing html

