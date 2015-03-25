{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module UI where

import Video
import RealWorld
import NameAlgo

import Control.Monad
import System.Exit
import Graphics.Vty
import Graphics.Vty.Widgets.All
import Data.IORef
import Data.List

type WL = Widget (List Video FormattedText)
type VidStat = ([Video], Video)
--              videos   last select video

data VList = VList WL (IORef [VidStat])

listW :: VList -> WL
listW (VList lw _) = lw

vidsVList :: [Video] -> IO VList
vidsVList vs = do
    lst <- newList 3
    forM_ vs $ \v -> addToList lst v =<< plainText (beaut v)
    ior <- newIORef []
    return $ VList lst ior

filterVList :: VList -> (Video -> Bool) -> IO ()
filterVList vl@(VList wl _) ok = do
    storeState vl
    setListVideos wl . filter ok =<< getListVideos wl

sortVList :: VList -> (Video -> Video -> Ordering) -> IO ()
sortVList vl@(VList wl _) cmp = do
    storeState vl
    setListVideos wl . sortBy cmp =<< getListVideos wl

storeState :: VList -> IO ()
storeState (VList wl st) = do
    vs <- getListVideos wl
    idx <- getWLSelected wl
    modifyIORef st ((vs, idx):)

setVList :: VList -> [Video] -> IO ()
setVList (VList wl st) vs = do
    oldVs <- getListVideos wl
    idx <- getWLSelected wl
    modifyIORef st ((oldVs, idx):)
    setListVideos wl vs

backVList :: VList -> IO ()
backVList (VList wl st) = 
    readIORef st >>= \case
        [] -> 
            return ()
        ((bvs, bloc):_) -> do
--            putStrLn "something >////<"
            setListVideosB wl bvs bloc
            modifyIORef st tail

tryExit :: Widget FocusGroup -> Key -> [Modifier] -> IO Bool
tryExit _ key _ = case key of
    KChar 'q' -> exitSuccess
    _         -> return False

fex :: Video -> IO Bool
fex itm
    | isVid itm = downloaded $ vidName itm
    | otherwise = return False

-- return the videos in the list
getListVideos :: WL -> IO [Video]
getListVideos lst = do
    sz <- getListSize lst
    forM [0..sz-1] $ \idx -> do
        Just (itm, _) <- getListItem lst idx
        return itm

getWLSelected :: WL -> IO Video
getWLSelected ls = do
    Just (_, (idx, _)) <- getSelected ls
    return idx

-- set lst content to vs
setListVideos :: WL -> [Video] -> IO ()
setListVideos lst vs = do
    oldItm <- getWLSelected lst
    clearList lst 
    forM_ vs $ \v -> addToList lst v =<< plainText (beaut v)
    listFindFirst lst oldItm >>= \case
        Just ind -> setSelected lst ind
        Nothing  -> return ()

setListVideosB :: WL -> [Video] -> Video -> IO ()
setListVideosB lst vs bvid = do
    Just (_, (oldItm, _)) <- getSelected lst
    clearList lst 
    forM_ vs $ \v -> addToList lst v =<< plainText (beaut v)
    listFindFirst lst oldItm >>= \case
        Just ind -> setSelected lst ind
        Nothing  -> listFindFirst lst bvid >>= \case
            Just ind2 -> setSelected lst ind2
            Nothing -> return ()

colorDecide :: WL -> IO ()
colorDecide lst = 
    onSelectionChange lst $ \sle -> case sle of
        SelectionOn _ itm _ -> if isFld itm
            then setFocusAttribute lst (black `on` magenta)
                    -- folder's color
            else fex itm >>= \case
                True  -> setFocusAttribute lst (black `on` red) 
                         -- if video have been download, use red color
                False -> setFocusAttribute lst (black `on` cyan)
                         -- if video haven't been download, use cyan
        _                   -> return ()

