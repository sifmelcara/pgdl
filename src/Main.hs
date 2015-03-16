{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import FetchHtml
import PrsVid
import NameAlgo 
import RealWorld
import Chkcfg
import Video
import Log
import GenStat
import CrtInf
import AskScene
import UI

import qualified Data.Text as T
import Data.IORef
import Data.Maybe
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

    tmpLst <- newList 3
    -- the main list will display on screen

    diskrd <- readVid
    -- read cache in the disk, if the cache do not exists, return "downloading"

    args <- getArgs
    let diskV = if null args then diskrd
                             else dlnVid
    -- if user want to search a video, then don't load cache to the screen 

    forM_ diskV $ \v -> addToList tmpLst v =<< plainText (beaut v)
    -- add disk videos to list

    tmpLfg <- newFocusGroup
    addToFocusGroup tmpLfg tmpLst

    statBar <- plainText waitingBar
    -- the state bar show in the bottom
    setNormalAttribute statBar (black `on` green)

    tmpUi <- centered =<< hFixed 80 =<< vBox tmpLst statBar
    -- ui consists of a list and a state bar
    chgTmpLs <- addToCollection c tmpUi tmpLfg
    

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


    lfg `onKeyPressed` tryExit
    dfcg `onKeyPressed` tryExit
    ifsfg `onKeyPressed` tryExit

    -- let's download video from internet !
    schedule $ do
        forkIO $ do
            rd <- prsHtm <$> fetchHtml
            -- prsHtm :: Text -> [Video (whether video or folder)]
            forkIO . writeVid $ rd
            -- write cache to the disk

            let vdlst = search rd args
            when (length vdlst < 1) $ error "no search result found (or it's empty)."
            
            setListVideos lst vdlst
            -- show new list

            ew <- editWidget
            -- a widget to enter keyword
            kfg <- newFocusGroup
            addToFocusGroup kfg ew
            kui <- centered =<< hFixed 80 =<< vBox lst ew
            chgky <- addToCollection c kui kfg
            -- create a scene presents a input box

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
                    
            let openFld lnk = do
                  ctnt <- (map (attcLink lnk) . prsHtm) <$> fetchFld lnk 
                  -- attach folder link to the videos in the folder
                  when (null ctnt) $ error "there is no video in the folder!" 
                  push
                  clearList lst 
                  forM_ ctnt $ \v -> addToList lst v =<< plainText (beaut v) 
                  return ()

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

            onActivate ew $ \e -> do
                t <- getEditText e
                setEditText e ""
                vs <- getListVideos lst
                push
                setListVideos lst $ search vs (words . T.unpack $ t)
                chgls
                -- return to the list

            onKeyPressed lst $ \_ key _ -> case key of
                KEnter -> do
                -- User can choose a folder or a video !
                    Just (_, (itm, _)) <- getSelected lst
                    case itm of
                        Folder _ lnk _ -> openFld lnk
                        v -> fex v >>= \case
                            True -> chgdl
                            False -> playVid itm
                    return True
                KLeft -> do
                -- return to the old list
                    pop
                    return True
                KChar 's' -> do
                -- filter the videos by the focused video
                    push
                    Just (_, (itm, _)) <- getSelected lst
                    vs <- getListVideos lst
                    setListVideos lst $ filter (\now -> isAlike itm now) vs
                    return True
                KChar 'n' -> do
                -- sort videos by their name
                    push
                    vs <- getListVideos lst
                    setListVideos lst $ sortByName vs
                    return True
                KChar '/' -> do
                    chgky
                    return True
                _   -> return False

            return ()
        return ()

    runUi c $ defaultContext { normalAttr = white `on` black 
                             , focusAttr  = black `on` blue
                             }

