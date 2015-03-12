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
    
    ew <- editWidget
    -- a widget to enter keyword
    kfg <- newFocusGroup
    addToFocusGroup kfg ew
    kui <- centered =<< hFixed 80 =<< vBox lst ew
    chgky <- addToCollection c kui kfg
    -- create a scene presents a input box

    onActivate ew $ \e -> do
        t <- getEditText e
        
        chgls
        -- return to the list

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
            
            setListVideos lst vdlst
            -- show new list

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

            astr <- newIORef []
            -- we store ancestors list element in the astr
            -- (those old list we leaved)
            let push = do
            -- store the state into astr
                  sz <- getListSize lst
                  Just (loc, _) <- getSelected lst
                  lem <- catMaybes <$> forM [0..sz-1] (getListItem lst)
                  modifyIORef astr ((lem, loc):)
            let pop = do
            -- pop a state from astr and give it to lst
                  ast <- readIORef astr
                  case ast of
                    [] -> return ()
                    ((itms, loc):_) -> do
                        clearList lst
                        forM_ itms $ \(v, w) -> addToList lst v w
                        setSelected lst loc
                        modifyIORef astr tail
                    
            let openFld lnk = do
                  ctnt <- (map (attcLink lnk) . prsHtm) <$> fetchFld lnk 
                  -- attach folder link to the videos in the folder
                  when (null ctnt) $ error "there is no video in the folder!" 
                  push
                  clearList lst 
                  forM_ ctnt $ \v -> addToList lst v =<< plainText (beaut v) 
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
                    pop
                    return True
                KChar 's' -> do
                    push
                    Just (_, (itm, _)) <- getSelected lst
                    size <- getListSize lst
                    let itemIdxs = reverse [0..size-1]
                    isLik <- forM itemIdxs $ \idx -> do
                        Just (now, _) <- getListItem lst idx
                        return $ isAlike itm now
                    forM_ (zip itemIdxs isLik) $ \(idx, lik) -> unless lik $ do
                        removeFromList lst idx
                        return ()
                    return True
                KChar '/' -> do
                    chgky
                    return True
                _   -> return False

            when (null args) $ void . forkIO . writeVid $ vdlst
            -- only write cache when user didn't search video

            return ()
        return ()

    runUi c $ defaultContext { normalAttr = white `on` black 
                             , focusAttr  = black `on` blue
                             }

    where tryExit _ key _ = case key of
            KChar 'q' -> exitSuccess
            _         -> return False
          fex itm
            | isVid itm = downloaded $ vidName itm
            | otherwise = return False
          -- return the videos in the list
          getListVideos lst = do
            sz <- getListSize lst
            forM [0..sz-1] $ \idx -> do
                Just (itm, _) <- getListItem lst idx
                return itm
          -- set lst content to vs
          setListVideos lst vs = do
            Just (_, (oldItm, _)) <- getSelected lst
            clearList lst 
            forM_ vs $ \v -> addToList lst v =<< plainText (beaut v)
            listFindFirst lst oldItm >>= \case
                Just ind -> setSelected lst ind
                Nothing  -> return ()


