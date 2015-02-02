{-# LANGUAGE OverloadedStrings #-}

module TestExist where

import Getconfig

import Data.Function
import System.Directory
import System.FilePath

import qualified Data.Text as T

downloaded :: T.Text -> IO Bool
downloaded t = do
    lcd <- getLocaldir
    doesFileExist $ (combine `on` T.unpack) lcd t


