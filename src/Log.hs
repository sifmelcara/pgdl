{-# LANGUAGE OverloadedStrings #-}

module Log where

import Video
import Data.Binary
import System.Directory
import System.FilePath
import Control.Applicative ((<$>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Binary
import qualified Data.ByteString.Lazy as B

instance Binary T.Text where
    put = put . T.encodeUtf8
    get = T.decodeUtf8 <$> get

instance Binary Video where
    put vid = do
        put $ vidName vid
    get = do
        name <- get
        return Video { vidName = name
                     , vidLink = ""
                     , vidDate = ""
                     , vidSize = ""
                     }

logname = ".pgdl.cache"

writeVid :: [Video] -> IO ()
writeVid vs = do
    hdir <- getHomeDirectory
    B.writeFile (hdir </> logname) $ encode vs  

readVid :: IO [Video]
readVid = do
    hdir <- getHomeDirectory
    let absdir = hdir </> logname
    fex <- doesFileExist absdir
    case fex of
        False -> do
            dat <- B.readFile (hdir </> logname)
            return $ decode dat
        True -> do
            return [Video { vidName = "Downloading..."
                         , vidLink = ""
                         , vidDate = ""
                         , vidSize = ""
                         }
                   ]
