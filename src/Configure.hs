{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Configure 
where

import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

getConfig :: IO C.Config
getConfig = C.load [C.Required "$(HOME)/.pgdl"]

getUsername :: IO (Maybe Text)
getUsername = do
    cfg <- getConfig
    C.lookup cfg "username" 

getPassword :: IO (Maybe Text)
getPassword = do 
    cfg <- getConfig
    C.lookup cfg "password"

getServpath :: IO (Maybe Text)
getServpath = do
    cfg <- getConfig
    C.lookup cfg "servpath"

getLocaldir :: IO (Maybe Text)
getLocaldir = do
    cfg <- getConfig
    C.lookup cfg "localdir" 

somethingWrong = error "Oops, it seems the config file (~/.pgdl) has something wrong."

