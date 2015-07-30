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
import Data.Ord
import Control.Applicative
import Control.Concurrent
import Control.Monad

import Graphics.Vty
import Graphics.Vty.Widgets.All

main :: IO ()
main = do
    chkcfg 
    -- check if there exists a configuration file, if not, then create one

    c <- newCollection

    tmpLst <- newList 3
    -- the main list will display on screen

    diskV <- readVid
    -- read cache in the disk, if the cache do not exists, return "downloading"

    forM_ diskV $ \v -> addToList tmpLst v =<< plainText (beaut v)
    -- add disk videos to list

    tmpLfg <- newFocusGroup
    addToFocusGroup tmpLfg tmpLst

    statBar <- plainText waitingBar
    -- the state bar show in the bottom
    setNormalAttribute statBar (black `on` green)

    tmpUi <- centered =<< hFixed 80 =<< vBox tmpLst statBar
    -- ui consists of a list and a state bar
    addToCollection c tmpUi tmpLfg
    

    (dlg, dfcg) <- newAskScene
    dui <- centered =<< hFixed 50 (sceneWidget dlg)
    chgdl <- addToCollection c dui dfcg

    ifsfg <- newFocusGroup
    -- a focus group for information page

    -- this function will show the focused item's information to user

    tmpLfg `onKeyPressed` tryExit
    dfcg `onKeyPressed` tryExit
    ifsfg `onKeyPressed` tryExit

    -- colorDecide tmpLst

    -- let's download video from internet !
    schedule $ do
        forkIO $ do
            vdlst <- prsHtm <$> fetchHtml
            -- prsHtm :: Text -> [Video (whether video or folder)]
            forkIO . writeVid $ vdlst
            -- write cache to the disk
            
            vlst <- vidsVList vdlst
            let lst = listW vlst
            
            -- move to the selected old item
            Just (_, (oldSeleItm, _)) <- getSelected tmpLst
            listFindFirst lst oldSeleItm >>= \case
                Just idx -> setSelected lst idx
                Nothing -> return ()

            -- show new list
            lfg <- newFocusGroup
            lfg `onKeyPressed` tryExit
            addToFocusGroup lfg lst
            lui <- centered =<< hFixed 80 =<< vBox lst statBar
            chgls <- addToCollection c lui lfg 
            chgls
            ew <- editWidget
            -- a widget to enter keyword
            kfg <- newFocusGroup
            addToFocusGroup kfg ew
            kui <- centered =<< hFixed 80 =<< vBox lst ew
            chgky <- addToCollection c kui kfg
            -- create a scene presents a input box

            colorDecide lst

            -- four button in ask scene's action
            onScePlay dlg $ \_ -> do
                Just (_, (itm, _)) <- getSelected lst
                justPlay itm
                chgls
            onSceDown dlg $ \_ -> do
                Just (_, (itm, _)) <- getSelected lst
                playVid itm
                chgls
            onSceQuit dlg $ const chgls
            onSceRemv dlg $ \_ -> do
                Just (_, (itm, _)) <- getSelected lst
                removeVid itm
                chgls

            onSelectionChange lst $ \sle -> case sle of
                SelectionOn _ itm _ -> setText statBar =<< genStat itm
                -- refresh state bar after any scroll
                _ -> return ()
            

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
                filterVList vlst $ \v -> vidInfx v (T.words t)
                chgls
                -- return to the list

            let openFld lnk = do
                  ctnt <- (map (attcLink lnk) . prsHtm) <$> fetchFld lnk 
                  -- attach folder link to the videos in the folder
                  when (null ctnt) $ error "there is no video in the folder!" 
                  setVList vlst ctnt
                  return ()

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
                    backVList vlst
                    return True
                KChar 's' -> do
                -- filter the videos by the focused video
                    Just (_, (itm, _)) <- getSelected lst
                    filterVList vlst (isAlike itm)
                    return True
                KChar 'n' -> do
                -- sort videos by their name
                    sortVList vlst (comparing getName) 
                    return True
                KChar 'v' -> do
                    filterVListIO vlst $ \obj -> if isVid obj then downloaded obj
                                                 else return False 
                    return True
                KChar 'u' -> do
                    filterVListIO vlst $ \obj -> if isVid obj then not <$> (downloaded obj)
                                                 else return False 
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

