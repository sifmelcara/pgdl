{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module UI where

import Video
import RealWorld

import Control.Monad
import NameAlgo
import System.Exit
import Graphics.Vty
import Graphics.Vty.Widgets.All
import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Data.List

type WL = Widget (List Video FormattedText)

data VList = VList WL (IORef [WL])

listW :: VList -> WL
listW (VList lw _) = lw

vidsVList :: [Video] -> IO VList
vidsVList vs = do
    lst <- newList 3
    forM_ vs $ \v -> addToList lst v =<< plainText (beaut v)
    ior <- newIORef []
    return $ VList lst ior

filterVList :: VList -> (Video -> Bool) -> IO ()
filterVList (VList wl st) ok = do
    modifyIORef st (wl:) 
    setListVideos wl . filter ok =<< getListVideos wl

sortVList :: VList -> (Video -> Video -> Ordering) -> IO ()
sortVList (VList wl st) cmp = do
    modifyIORef st (wl:)
    setListVideos wl . sortBy cmp =<< getListVideos wl

setVList :: VList -> [Video] -> IO ()
setVList (VList wl st) vs = do
    modifyIORef st (wl:)
    setListVideos wl vs

backVList :: VList -> IO ()
backVList (VList wl st) =
    readIORef st >>= \case
        [] -> return ()
        (blst:_) -> do
            setListVideos wl =<< getListVideos blst
            modifyIORef st tail


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

-- set lst content to vs
setListVideos :: WL -> [Video] -> IO ()
setListVideos lst vs = do
    Just (_, (oldItm, _)) <- getSelected lst
    clearList lst 
    forM_ vs $ \v -> addToList lst v =<< plainText (beaut v)
    listFindFirst lst oldItm >>= \case
        Just ind -> setSelected lst ind
        Nothing  -> return ()

