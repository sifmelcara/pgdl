{-# LANGUAGE OverloadedStrings #-}
module Types
where

import Text.HTML.DirectoryListing.Type
import Data.Text (Text)

data DNode = Directory Entry (IO [DNode]) | File Entry Text

