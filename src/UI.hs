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

type WL = Widget (List Text FormattedText)

data VList = VList WL (IORef [WL])

vidsVList :: IO VList
newVList vs = do
    

tryExit _ key _ = case key of
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
