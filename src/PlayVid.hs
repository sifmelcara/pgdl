{-# LANGUAGE OverloadedStrings #-}

module PlayVid where

import Getconfig
import Video

import Control.Monad
import Distribution.System
import System.Process
import System.Directory
import System.FilePath.Posix
import System.IO
import System.Exit
import qualified Data.Text as T

playVid :: Video -> IO ()
playVid vid = do
    username <- getUsername
    password <- getPassword
    servpath <- getServpath
    localdir <- getLocaldir

    let localloc = localdir </> vn
    fex <- doesFileExist localdir
    when fex $ removeFile localloc

    let purepath = reverse . dropWhile (/= '/') . reverse $ servpath
    case username of
        ""  -> let url = "http://" ++                                       purepath ++ vu
               in runCommand $ "nohup curl " ++ addq url ++ " -o " ++ addq localloc ++ "&>/dev/null &"
        _   -> let url = "http://" ++ username ++ ":" ++ password ++ "@" ++ purepath ++ vu
               in runCommand $ "nohup curl " ++ addq url ++ " -o " ++ addq localloc ++ "&>/dev/null &"
    let checkFile = doesFileExist localloc >>= \ready -> unless ready checkFile
    checkFile
    case buildOS of
        OSX   -> runCommand $ "open " ++ addq localloc ++ " -a vlc"
        Linux -> runCommand $ "nohup vlc -f " ++ addq localloc ++ " &>/dev/null &"
        _     -> error "OS unsupported!"
    exitSuccess
  where vn = T.unpack . vidName $ vid
        vu = T.unpack . vidLink $ vid
        addq s = "\"" ++ s ++ "\""

justPlay :: Video -> IO ()
justPlay vid = do
    servpath <- getServpath
    localdir <- getLocaldir
    let localloc = localdir </> vn
    case buildOS of
        OSX   -> runCommand $ "open " ++ addq localloc ++ " -a vlc"
        Linux -> runCommand $ "nohup vlc -f " ++ addq localloc ++ " &>/dev/null &"
        _     -> error "OS unsupported!"
    exitSuccess
  where vn = T.unpack . vidName $ vid
        addq s = "\"" ++ s ++ "\""
    
    



