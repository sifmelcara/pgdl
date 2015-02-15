{-# LANGUAGE OverloadedStrings #-}

module Getconfig where

import Control.Monad
import Data.Maybe

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

getUsername :: IO String
getUsername = fmap (\(r, _, _, _) -> r) getConfig

getPassword :: IO String
getPassword = fmap (\(_, r, _, _) -> r) getConfig

getServpath :: IO String
getServpath = fmap (\(_, _, r, _) -> r) getConfig

getLocaldir :: IO String
getLocaldir = fmap (\(_, _, _, r) -> r) getConfig

