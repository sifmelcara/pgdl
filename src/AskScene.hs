{-# LANGUAGE OverloadedStrings #-}

module AskScene where

import Graphics.Vty.Widgets.All
import Graphics.Vty.Input.Events

import Control.Monad

data AskScene = AskScene { sceneWidget :: Widget (Bordered Padded)
                         , playHand :: Handlers AskScene
                         , downHand :: Handlers AskScene
                         , quitHand :: Handlers AskScene
                         , remvHand :: Handlers AskScene
                         }

newAskScene :: IO (AskScene, Widget FocusGroup)
newAskScene = do
    playB <- newButton "No"
    downB <- newButton "Yes"
    quitB <- newButton "Quit"
    remvB <- newButton "Remov"
    buttonBox <- mkwIO playB <++> mkwIO downB <++> mkwIO quitB <++> mkwIO remvB
    b <- do
        bx <- plainText "    Download it again?" <--> return buttonBox
        setBoxSpacing bx 1
        withPadding (padAll 1) bx
    fg <- newFocusGroup
    mapM_ (addToFocusGroup fg . buttonWidget) [playB, downB, quitB, remvB]
    ui <- withBorderedLabel "File Exists!" =<< bordered b
    [phs, dhs, qhs, rhs] <- replicateM 4 newHandlers
    let sce = AskScene { sceneWidget = ui
                       , playHand = phs
                       , downHand = dhs
                       , quitHand = qhs
                       , remvHand = rhs
                       }
    onButtonPressed playB $ \_ -> 
        fireEvent sce (return . playHand) sce
    onButtonPressed downB $ \_ -> 
        fireEvent sce (return . downHand) sce
    onButtonPressed quitB $ \_ -> 
        fireEvent sce (return . quitHand) sce
    onButtonPressed remvB $ \_ ->
        fireEvent sce (return . remvHand) sce
    setFocusGroupNextKey fg KRight []
    setFocusGroupPrevKey fg KLeft []
    return (sce, fg)
    where mkwIO = return . buttonWidget

onScePlay :: AskScene -> Handler AskScene -> IO ()
onScePlay = addHandler (return . playHand)

onSceDown :: AskScene -> Handler AskScene -> IO ()
onSceDown = addHandler (return . downHand)

onSceQuit :: AskScene -> Handler AskScene -> IO ()
onSceQuit = addHandler (return . quitHand)

onSceRemv :: AskScene -> Handler AskScene -> IO()
onSceRemv = addHandler (return . remvHand)

