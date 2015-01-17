{-# LANGUAGE OverloadedStrings #-}

module PlayVid where

import Getconfig

import System.Process
import System.Directory
import Control.Monad
import qualified Data.Text as T
import System.IO
import Video

playVid :: Video -> IO ()
playVid vid = do
    runCommand $ "rm " ++ addq vn

    username <- fmap T.unpack getUsername
    password <- fmap T.unpack getPassword
    servpath <- fmap T.unpack getServpath
    localdir <- fmap T.unpack getLocaldir
    let localloc = localdir ++ vn
    let purepath = reverse . dropWhile (/= '/') . reverse $ servpath
    let url = "http://" ++ username ++ ":" ++ password ++ "@" ++ purepath ++ vu
    runCommand $ "nohup curl " ++ addq url ++ " -o " ++ addq localloc ++ "&>/dev/null &"
    let checkFile = doesFileExist localloc >>= \ready -> unless ready checkFile
    checkFile
    runCommand $ "nohup vlc -f " ++ addq localloc ++ " &>/dev/null &"
    return ()
  where vn = T.unpack . vidName $ vid
        vu = T.unpack . vidLink $ vid
        addq s = "\"" ++ s ++ "\""


