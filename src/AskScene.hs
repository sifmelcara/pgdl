{-# LANGUAGE OverloadedStrings #-}

module AskScene where

import Graphics.Vty.Widgets.All

import qualified Data.Text as T

data AskScene = AskScene { SceneWidget :: Widget Bordered
                         }

newAskScene :: IO (Widget AskScene)
newAskScene = do
    playB <- newBotton "Play it"
    downB <- newBotton "Download and Play"
    quitB <- newBotton "Quit"
    buttonBox <- (return playB) <++> 
                 (return downB) <++>
                 (return quitB)
    b <- (return $ plainText "What to do?") <--> buttonBox
    fg <- newFocusGroup
    mapM_ (addToFocusGroup fg . buttonWidget) [playB, downB, quitB]
    ui <- withBorderedLabel "File Exists!" =<< bordered b


