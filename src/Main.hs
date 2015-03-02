{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import FetchHtml
import PrsVid
import Beaut
import PlayVid
import Search
import Chkcfg
import Video
import Log
import TestExist
import GenStat
import CrtInf
import AskScene

import Control.Applicative
import Control.Concurrent
import Control.Monad
import System.Exit
import System.Environment

import Graphics.Vty
import Graphics.Vty.Widgets.All

main :: IO ()
main = do
    chkcfg

    c <- newCollection

    lst <- newList 3 
    diskrd <- readVid
    args <- getArgs
    let diskV = if null args then diskrd
                else dlnVid
    forM_ diskV $ \v -> addToList lst v =<< plainText (beaut v)
    lfg <- newFocusGroup
    addToFocusGroup lfg lst

    statBar <- plainText waitingBar
    setNormalAttribute statBar (black `on` green)

    ui <- centered =<< hFixed 80 =<< vBox lst statBar
    chgls <- addToCollection c ui lfg


    (dlg, dfcg) <- newAskScene
    dui <- centered =<< hFixed 50 (sceneWidget dlg)
    chgdl <- addToCollection c dui dfcg

    dfcg  `onKeyPressed` tryExit

    lfg   `onKeyPressed` tryExit
    
    -- four button in ask scene's action
    onScePlay dlg $ \_ -> do
        Just (_, (itm, _)) <- getSelected lst
        justPlay itm
    onSceDown dlg $ \_ -> do
        Just (_, (itm, _)) <- getSelected lst
        playVid itm
    onSceQuit dlg $ const exitSuccess
    onSceRemv dlg $ \_ -> do
        Just (_, (itm, _)) <- getSelected lst
        removeVid itm

    ifsfg <- newFocusGroup
    onKeyPressed ifsfg $ \_ key _ -> case key of
        KLeft -> chgls >> return True   
        _     -> return False
    ifsfg `onKeyPressed` tryExit
    onKeyPressed lst $ \_ key _ -> case key of
        KRight -> do
            Just (_, (itm, _)) <- getSelected lst
            inf <- centered =<< plainText (crtInfPg itm)
            addToFocusGroup ifsfg inf
            join $ addToCollection c inf ifsfg
            return True
        _ -> return False
    schedule $ do
        forkIO $ do
            rd <- prsHtm <$> fetchHtml
            let vdlst = search rd args
            when (length vdlst < 1) $ error "no video or folder found!"
            Just (_, (oldItm, _)) <- getSelected lst
            clearList lst
            forM_ vdlst $ \v -> addToList lst v =<< plainText (beaut v)
            listFindFirst lst oldItm >>= \case
                Just ind -> setSelected lst ind
                Nothing  -> return ()

            onSelectionChange lst $ \sle -> case sle of
                SelectionOn _ itm _ -> fex itm >>= \case 
                    True  -> setFocusAttribute lst (black `on` red) 
                    False -> setFocusAttribute lst (black `on` cyan)
                _                   -> return ()
            onSelectionChange lst $ \sle -> case sle of
                SelectionOn _ itm _ -> setText statBar =<< genStat itm
                _ -> return ()

            (null <$> getArgs) >>= \case                
                True -> void . forkIO $ writeVid vdlst
                _    -> return ()
            return ()
        return ()

    let openFld lnk = do
            ctnt <- (map (attcLink lnk) . prsHtm) <$> fetchFld lnk 
            when (null ctnt) $ error "no video in the folder!" 
            clearList lst 
            forM_ ctnt $ \v -> addToList lst v =<< plainText (beaut v) 
            return () 

    -- User chose a folder or video !
    onKeyPressed lst $ \_ key _ -> case key of
        KEnter -> do
            Just (_, (itm, _)) <- getSelected lst
            case itm of
                Folder _ lnk _ -> openFld lnk
                v -> fex v >>= \case
                    True -> chgdl
                    False -> playVid itm
            return True
        _   -> return False
            

    runUi c $ defaultContext {normalAttr = white `on` black, 
                              focusAttr  = black `on` blue
                             }
    where tryExit _ key _ = case key of
            KChar 'q' -> exitSuccess
            _         -> return False

fex itm
    | isVid itm = downloaded $ vidName itm
    | otherwise = return False

