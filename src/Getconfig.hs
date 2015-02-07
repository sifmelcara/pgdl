{-# LANGUAGE OverloadedStrings #-}

module Getconfig where

import Control.Monad
import Data.Maybe

import qualified Data.Text as T
import qualified Data.Configurator as C

getConfig :: IO (String, String, String, String)
getConfig = do
    config <- C.load [C.Required "$(HOME)/.pgdl"]
    username <- C.lookup config "username"
    password <- C.lookup config "password"
    servpath <- C.lookup config "servpath"
    localdir <- C.lookup config "localdir"
    mapM_ (\x -> when (isNothing x) $ error "please edit config file.") [username, password, servpath, localdir]
    return (fj username, fj password, fj servpath, fj localdir)
    where fj = fromJust

getUsername = fmap (\(r, _, _, _) -> r) getConfig
getPassword = fmap (\(_, r, _, _) -> r) getConfig
getServpath = fmap (\(_, _, r, _) -> r) getConfig
getLocaldir = fmap (\(_, _, _, r) -> r) getConfig

