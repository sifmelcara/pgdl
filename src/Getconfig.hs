{-# LANGUAGE OverloadedStrings #-}

module Getconfig where

import qualified Data.Text as T
import qualified Data.Configurator as C
import Data.Maybe

getConfig :: IO (T.Text, T.Text, T.Text)
getConfig = do
    config <- C.load [C.Required "$(HOME)/.pgdl"]
    username <- C.lookup config "username"
    password <- C.lookup config "password"
    servpath <- C.lookup config "servpath"
    return (fj username, fj password, fj servpath)
    where fj = fromJust

getUsername = fmap (\(r, _, _) -> r) getConfig
getPassword = fmap (\(_, r, _) -> r) getConfig
getServpath = fmap (\(_, _, r) -> r) getConfig

