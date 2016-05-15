{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Configure 
where

import qualified Data.Text as T
import Data.Text (Text)
import Control.Applicative

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

import System.FilePath.Posix
import System.Directory

getConfig :: IO (Maybe C.Config)
getConfig = do
    home <- getUserDocumentsDirectory
    let cfgFile = home </> ".pgdl"
    doesFileExist cfgFile >>= \case
        False -> return Nothing
        True -> Just <$> C.load [C.Required cfgFile]
    
getUsername :: IO (Maybe Text)
getUsername =
    getConfig >>= \case
        Nothing -> return Nothing
        Just cfg -> C.lookup cfg "username" 


getPassword :: IO (Maybe Text)
getPassword = 
    getConfig >>= \case
        Nothing -> return Nothing
        Just cfg -> C.lookup cfg "password" 

getLocaldir :: IO (Maybe Text)
getLocaldir =
    getConfig >>= \case
        Nothing -> return Nothing
        Just cfg -> C.lookup cfg "localdir" 

-- | return Nothing if there are no servpath or config file do not exist
getServpath :: IO (Maybe Text)
getServpath = do
    cfg <- getConfig
    case cfg of
        Nothing -> return Nothing
        Just c -> fmap unify <$> C.lookup c "servpath"
    where
    -- this is not a good approach, improve it later
    unify :: Text -> Text
    unify p
        | "http://" `T.isPrefixOf` p = p
        | "https://" `T.isPrefixOf` p = p
        | otherwise = "http://" `T.append` p

-- | This function also creates the cache directory
-- if it doesn't exists.
getCacheFileLocation :: IO FilePath
getCacheFileLocation = do
    home <- getUserDocumentsDirectory
    let cdir = home </> ".cache"
    createDirectoryIfMissing True cdir
    let cacheFile = cdir </> "pgdl"
    return cacheFile

