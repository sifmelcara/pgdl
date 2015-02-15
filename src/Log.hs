{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Log where

import Video

import System.Directory
import System.FilePath

import qualified Data.Text as T
import qualified Data.Text.IO as T

encVid :: [Video] -> T.Text
encVid vs = T.intercalate "\n" $ map vidName vs

decVid :: T.Text -> [Video]
decVid = map (\t -> Video t "" "" "") . T.lines

logname :: FilePath
logname = ".pgdl.cache"

writeVid :: [Video] -> IO ()
writeVid vs = do
    hdir <- getHomeDirectory
    T.writeFile (hdir </> logname) $ encVid vs 

readVid :: IO [Video]
readVid = do
    hdir <- getHomeDirectory
    let absdir = hdir </> logname
    doesFileExist absdir >>= \case
        True -> do
            dat <- T.readFile (hdir </> logname)
            return $ decVid dat
        False -> return dlnVid

dlnVid :: [Video]
dlnVid = [Video "Downloading..." "" "" ""]


