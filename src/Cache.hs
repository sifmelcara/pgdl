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

import Fetcher

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

readCache :: Text -> IO (Maybe [DNode])
readCache rootUrl = do
    p <- getCacheFileLocation
    decodeFileOrFail p >>= \case
        Right d -> return . Just . map toDNode $ d
        Left _ -> return Nothing
    where
    toDNode :: Entry -> DNode
    toDNode e
        | isDirectory e = Directory e noData
        | otherwise = File e (rootUrl `T.append` href e)
    noData = error "offline mode"
    
