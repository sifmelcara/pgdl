{-# LANGUAGE OverloadedStrings #-}

module PlayVid where

import Getconfig

import System.Process
import System.Directory
import Control.Monad
import qualified Data.Text as T
import System.IO

playVid :: (T.Text, T.Text, T.Text) -> IO ()
playVid (tvn, tvu, _) = do
    runCommand $ "rm " ++ addq vn

    username <- fmap T.unpack getUsername
    password <- fmap T.unpack getPassword
    servpath <- fmap T.unpack getServpath
    let purepath = reverse . dropWhile (/= '/') . reverse $ servpath
    let url = "http://" ++ username ++ ":" ++ password ++ "@" ++ purepath ++ vu
    runCommand $ "nohup curl " ++ addq url ++ " -o " ++ addq vn ++ "&>/dev/null &"
    checkFile
    runCommand $ "nohup vlc -f " ++ addq vn ++ " &>/dev/null &"
    return ()
  where vn = T.unpack tvn
        vu = T.unpack tvu
        checkFile = doesFileExist vn >>= \ready -> unless ready checkFile
        addq s = "\"" ++ s ++ "\""


