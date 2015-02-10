{-# LANGUAGE OverloadedStrings #-}

module AskScene where

import Graphics.Vty.Widgets.All

import qualified Data.Text as T
import Control.Monad

data AskScene = AskScene { sceneWidget :: Widget (Bordered Padded)
                         , playHand :: Handlers AskScene
                         , downHand :: Handlers AskScene
                         , quitHand :: Handlers AskScene
                         }

newAskScene :: IO (AskScene, Widget FocusGroup)
newAskScene = do
    playB <- newButton "No"
    downB <- newButton "Yes"
    quitB <- newButton "Quit"
    buttonBox <- (mkwIO playB) <++> (mkwIO downB) <++> (mkwIO quitB)
    --setBoxSpacing buttonBox 4
    b <- withPadding (padAll 1) =<< (plainText "Download it again?") <--> (return buttonBox)
    fg <- newFocusGroup
    mapM_ (addToFocusGroup fg . buttonWidget) [playB, downB, quitB]
    ui <- withBorderedLabel "File Exists!" =<< bordered b
    [phs, dhs, qhs] <- replicateM 3 newHandlers
    let sce = AskScene { sceneWidget = ui
                       , playHand = phs
                       , downHand = dhs
                       , quitHand = qhs
                       }
    onButtonPressed playB $ \_ -> 
        fireEvent sce (return . playHand) sce
    onButtonPressed downB $ \_ -> 
        fireEvent sce (return . downHand) sce
    onButtonPressed quitB $ \_ -> 
        fireEvent sce (return . quitHand) sce
    return (sce, fg)
    where mkwIO = return . buttonWidget

onScePlay :: AskScene -> Handler AskScene -> IO ()
onScePlay = addHandler (return . playHand)

onSceDown :: AskScene -> Handler AskScene -> IO ()
onSceDown = addHandler (return . downHand)

onSceQuit :: AskScene -> Handler AskScene -> IO ()
onSceQuit = addHandler (return . quitHand)



