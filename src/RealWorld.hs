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
import qualified Data.Text as T

-- play a video
playVid :: Video -> IO ()
playVid vid = do
    when (isFld vid) $ error "give playVid a folder."

    -- don't download a video whoose name is empty
    when ("/" `T.isSuffixOf` vidLink vid) $ error "maybe this is not a video?"

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
        OSX   -> runCommand $ "open " ++ addq localloc ++ " "
        Linux -> runCommand $ "nohup xdg-open " ++ addq localloc ++ " &>/dev/null &"
        _     -> error "OS unsupported!"
    return ()
  where vn = T.unpack . vidName $ vid
        vu = T.unpack . vidLink $ vid
        addq :: String -> String
        addq s = "\"" ++ s ++ "\""

justPlay :: Video -> IO ()
justPlay vid = do
    when (isFld vid) $ error "give playVid a folder."

    localdir <- getLocaldir
    let localloc = localdir </> vn
    case buildOS of
        OSX   -> runCommand $ "open " ++ addq localloc ++ " "
        Linux -> runCommand $ "nohup xdg-open " ++ addq localloc ++ " &>/dev/null &"
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
downloaded :: Video -> IO Bool
downloaded v = do
    lcd <- getLocaldir
    doesFileExist $ lcd </> T.unpack t
    where t = if isVid v then vidName v
              else error "try to consider a folder downloaded or not" 

