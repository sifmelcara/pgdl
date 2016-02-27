{-# LANGUAGE OverloadedStrings #-}
module Main
where

import qualified Data.Text as T
import Networking

main = do
    w <- getWebpage "https://www.kernel.org/pub/"
    print w

