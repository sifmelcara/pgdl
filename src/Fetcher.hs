module Fetcher
where

import Configure
import Networking
import Text.HTML.DirectoryListing.Type

fetchRoot :: IO [Entry]
fetchRoot = do
    root <- getServpath

