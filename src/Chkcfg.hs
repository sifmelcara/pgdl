
module Chkcfg where

import System.Directory
import Control.Monad

chkcfg = do
    flp <- fmap (++ "/.pgdl") getHomeDirectory
    fle <- doesFileExist flp
    unless fle $ do
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

