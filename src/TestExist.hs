{-# LANGUAGE OverloadedStrings #-}

module TestExist where

import Getconfig

import System.Directory
import System.FilePath

import qualified Data.Text as T

downloaded :: T.Text -> IO Bool
downloaded t = do
    lcd <- getLocaldir
    doesFileExist $ lcd </> T.unpack t


