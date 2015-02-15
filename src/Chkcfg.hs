
module Chkcfg (chkcfg) where

import System.Directory
import System.FilePath
import Control.Monad

chkcfg :: IO ()
chkcfg = do
    flp <- fmap (</> ".pgdl") getHomeDirectory
    fle <- doesFileExist flp
    unless fle $ do
              writeFile flp defcfg
              error "please config the file \"~/.pgdl\" first!"
    where defcfg = unlines [ ""
                           , "# example: "
                           , "# username = \"jack\" "
                           , "# password = \"mypassw\" "
                           , "# servpath = \"example.org/videodir/\" "
                           , "# localdir = \"/home/jack/Downloads/\" "
                           , ""
                           , "# username and localdir is optional."
                           , ""
                           , "username = \"\""
                           , "password = \"\""
                           , "servpath = \"\""
                           , "localdir = \"\""
                           , ""
                           ]

