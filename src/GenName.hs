{-# LANGUAGE OverloadedStrings #-}

module GenName where

import Network.HTTP.Base
import qualified Data.Text as T

genName :: T.Text -> T.Text
genName = T.pack . urlDecode . T.unpack

