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
    username <- fmap T.unpack getUsername
    password <- fmap T.unpack getPassword
    servpath <- fmap T.unpack getServpath
    localdir <- fmap T.unpack getLocaldir

    let localloc = localdir </> vn
    fex <- doesFileExist localdir
    when fex $ removeFile localloc

    let purepath = reverse . dropWhile (/= '/') . reverse $ servpath
    let url = "http://" ++ username ++ ":" ++ password ++ "@" ++ purepath ++ vu
    runCommand $ "nohup curl " ++ addq url ++ " -o " ++ addq localloc ++ "&>/dev/null &"
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


