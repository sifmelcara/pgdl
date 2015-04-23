{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module RealWorld where

import Getconfig
import Video

import Control.Monad
import Distribution.System
import System.Process
import System.Directory
import System.FilePath.Posix
import System.Exit
import qualified Data.Text as T

-- play a video
playVid :: Video -> IO ()
playVid vid = do
    when (isFld vid) $ error "give playVid a folder."
                 
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
    return ()
    -- exitSuccess
  where vn = T.unpack . vidName $ vid
        vu = T.unpack . vidLink $ vid
        addq :: String -> String
        addq s = "\"" ++ s ++ "\""

justPlay :: Video -> IO ()
justPlay vid = do
    localdir <- getLocaldir
    let localloc = localdir </> vn
    case buildOS of
        OSX   -> runCommand $ "open " ++ addq localloc ++ " -a vlc"
        Linux -> runCommand $ "nohup vlc -f " ++ addq localloc ++ " &>/dev/null &"
        _     -> error "OS unsupported!"
    return ()
    -- exitSuccess
  where vn = T.unpack . vidName $ vid
        addq :: String -> String
        addq s = "\"" ++ s ++ "\""
    
-- | remove a video 
removeVid :: Video -> IO ()
removeVid vid = do
    localDir <- getLocaldir
    removeFile $ localDir </> vn
    return ()
    -- exitSuccess
    where vn = T.unpack . vidName $ vid

-- | determine whether a video is downloaded
downloaded :: T.Text -> IO Bool
downloaded t = do
    lcd <- getLocaldir
    doesFileExist $ lcd </> T.unpack t

