{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Getconfig where

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

getConfig :: IO C.Config
getConfig = C.load [C.Required "$(HOME)/.pgdl"]

getUsername :: IO String
getUsername = do
    cfg <- getConfig
    C.lookup cfg "username" >>= \case
        Nothing -> noArg
        Just s -> return s

getPassword :: IO String
getPassword = do 
    cfg <- getConfig
    C.lookup cfg "password" >>= \case
        Nothing -> noArg
        Just s -> return s

getServpath :: IO String
getServpath = do
    cfg <- getConfig
    C.lookup cfg "servpath" >>= \case
        Nothing -> noArg
        Just s -> return s

getLocaldir :: IO String
getLocaldir = do
    cfg <- getConfig
    C.lookup cfg "localdir" >>= \case
        Nothing -> noArg
        Just s -> return s

noArg :: IO String
noArg = error "please correct the config file (~/.pgdl)."

