
module Chkcfg where

import System.Directory

chkcfg = do
    flp <- fmap (++ "/.pgdl") getHomeDirectory
    fle <- doesFileExist flp
    if fle then return ()
           else do
              writeFile flp defcfg
              error "please config file at ~/.pgdl first!"

defcfg = unlines [  "",
                    "# example:",
                    "# username = jack",
                    "# password = mypassw",
                    "# servpath = \"example.org/videodir/?C=M;O=D\"",
                    "",
                    "username = ",
                    "password = ",
                    "servpath = ",
                    ""
                 ]

