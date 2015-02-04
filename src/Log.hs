{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Log where

import Video

import Control.Applicative 
import Control.Monad
import Data.Binary
import Data.Either
import System.Directory
import System.FilePath

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as B

instance Binary T.Text where
    put = put . T.encodeUtf8
    get = T.decodeUtf8 <$> get

instance Binary Video where
    put vid = put $ vidName vid
    get = get >>= \name -> return $ Video name "" "" ""

logname = ".pgdl.cache"

writeVid :: [Video] -> IO ()
writeVid vs = do
    hdir <- getHomeDirectory
    B.writeFile (hdir </> logname) $ encode vs  

readVid :: IO [Video]
readVid = do
    hdir <- getHomeDirectory
    let absdir = hdir </> logname
    doesFileExist absdir >>= \case
        True -> do
            dat <- B.readFile (hdir </> logname)
            let res = decodeOrFail dat
            case res of
                Right (_, _, vs) -> return vs
                _                -> return dlnVid
        False -> return dlnVid
    where dlnVid = [Video "Downloading..." "" "" ""]

