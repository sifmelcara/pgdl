{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Cache
where

import Configure
import qualified Data.Text as T
import Data.Text (Text)
import Text.HTML.DirectoryListing.Type
import Data.Binary
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import System.Directory
import Data.Maybe

import Types 
import Local

-- | this conversion may lose accuracy?
instance Binary LocalTime where
    put = put . toRational . utcTimeToPOSIXSeconds . localTimeToUTC utc
    get = utcToLocalTime utc . posixSecondsToUTCTime . fromRational <$> get

instance Binary Entry where
    put e = do
        put $ visibleName e
        put $ href e
        put $ lastModified e
        put $ fileSize e
    get = do
        v <- get
        h <- get
        l <- get
        f <- get
        return $ Entry { visibleName = v
                       , href = h
                       , lastModified = l
                       , fileSize = f
                       }
        
writeCache :: [Entry] -> IO ()
writeCache es = do
    p <- getCacheFileLocation
    encodeFile p es

readCache :: IO (Maybe [DNode])
readCache = do
    p <- getCacheFileLocation
    lcd <- T.unpack . fromMaybe "" <$> getLocaldir
    doesFileExist p >>= \case
        False -> return Nothing
        True -> do
            decodeFileOrFail p >>= \case
                Right d -> do
                    dnodes <- mapM (toDNode lcd) d
                    return $ Just dnodes
                Left _ -> return Nothing
    where
    toDNode :: String -> Entry -> IO DNode
    toDNode lcd e
        | isDirectory e = return $ Directory e noData
        | otherwise = do
            downloaded <- isFileDownloaded (decodedName e) lcd
            return $ File e "offline mode" downloaded
    noData = error "offline mode"
    
