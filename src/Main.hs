{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import FetchHtml
import PrsVid
import NameAlgo 
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
    -- check if there exists a configuration file, if not, then create one

    c <- newCollection

    lst <- newList 3
    -- the main list will display on screen

    diskrd <- readVid
    -- read cache in the disk, if the cache do not exists, return "downloading"

    args <- getArgs
    let diskV = if null args then diskrd
                             else dlnVid
    -- if user want to search a video, then don't load cache to the screen 

    forM_ diskV $ \v -> addToList lst v =<< plainText (beaut v)
    -- add disk videos to list

    lfg <- newFocusGroup
    addToFocusGroup lfg lst

    statBar <- plainText waitingBar
    -- the state bar show in the bottom
    setNormalAttribute statBar (black `on` green)

    ui <- centered =<< hFixed 80 =<< vBox lst statBar
    -- ui consists of a list and a state bar
    chgls <- addToCollection c ui lfg

    (dlg, dfcg) <- newAskScene
    dui <- centered =<< hFixed 50 (sceneWidget dlg)
    chgdl <- addToCollection c dui dfcg

    
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
    -- a focus group for information page

    -- this function will show the focused item's information to user
    let chgInf = do
          Just (_, (vid, _)) <- getSelected lst
          inf <- centered =<< plainText (crtInfPg vid)
          addToFocusGroup ifsfg inf
          join $ addToCollection c inf ifsfg

    -- the information scene
    onKeyPressed ifsfg $ \_ key _ -> case key of
        KLeft -> do
            chgls -- return to the list
            return True
        KUp   -> do
            scrollUp lst
            chgInf
            return True
        KDown -> do
            scrollDown lst
            chgInf
            return True
        _     -> return False


    onKeyPressed lst $ \_ key _ -> case key of
        KRight -> do
            chgInf
            return True
        _ -> return False

    lfg `onKeyPressed` tryExit
    dfcg `onKeyPressed` tryExit
    ifsfg `onKeyPressed` tryExit

    -- let's download video from internet !
    schedule $ do
        forkIO $ do
            rd <- prsHtm <$> fetchHtml
            -- prsHtm :: Text -> [Video (whether video or folder)]

            let vdlst = search rd args
            when (length vdlst < 1) $ error "no search result found (or it's empty)."

            Just (_, (oldItm, _)) <- getSelected lst
            -- store the location before page refresh

            clearList lst
            forM_ vdlst $ \v -> addToList lst v =<< plainText (beaut v)
            -- show new list

            listFindFirst lst oldItm >>= \case
                Just ind -> setSelected lst ind
                Nothing  -> return ()

            onSelectionChange lst $ \sle -> case sle of
                SelectionOn _ itm _ -> case isFld itm of
                    True -> setFocusAttribute lst (black `on` magenta)
                            -- folder's color
                    _    -> fex itm >>= \case
                        True  -> setFocusAttribute lst (black `on` red) 
                                 -- if video have been download, use red color
                        False -> setFocusAttribute lst (black `on` cyan)
                                 -- if video haven't been download, use cyan
                _                   -> return ()

            onSelectionChange lst $ \sle -> case sle of
                SelectionOn _ itm _ -> setText statBar =<< genStat itm
                -- refresh state bar after any scroll
                _ -> return ()

            when (null args) $ void . forkIO . writeVid $ vdlst
            -- only write cache when user didn't search video

            return ()
        return ()

    let openFld lnk = do
            ctnt <- (map (attcLink lnk) . prsHtm) <$> fetchFld lnk 
            -- attach folder link to the videos in the folder
            when (null ctnt) $ error "there is no video in the folder!" 
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
            

    runUi c $ defaultContext { normalAttr = white `on` black 
                             , focusAttr  = black `on` blue
                             }

    where tryExit _ key _ = case key of
            KChar 'q' -> exitSuccess
            _         -> return False
          fex itm
            | isVid itm = downloaded $ vidName itm
            | otherwise = return False


