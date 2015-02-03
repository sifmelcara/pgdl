{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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

import Control.Applicative
import Control.Concurrent
import Control.Monad
import System.Directory
import System.Exit
import System.Environment
import System.FilePath
import System.IO

import Graphics.Vty
import Graphics.Vty.Widgets.All
import qualified Data.Text as T

main = do
    chkcfg

    c <- newCollection

    lst <- newList 3 
    diskrd <- readVid
    diskV  <- search diskrd <$> getArgs
    forM_ diskV $ \v -> addToList lst v =<< plainText (beaut v)
    lfg <- newFocusGroup
    addToFocusGroup lfg lst

    statBar <- plainText ""
    setNormalAttribute statBar (black `on` green)

    ui <- centered =<< hFixed 80 =<< vBox lst statBar
    chgls <- addToCollection c ui lfg


    (dlg, dfcg) <- do
        wg <- plainText "redownload it?"
        newDialog wg "File Exists!"
    dui <- centered =<< hFixed 30 (dialogWidget dlg)
    chgdl <- addToCollection c dui dfcg

    setFocusGroupNextKey dfcg KRight []
    setFocusGroupPrevKey dfcg KLeft  []
    dfcg  `onKeyPressed` tryExit

    lfg   `onKeyPressed` tryExit
                                            
    dlg `onDialogCancel` const exitSuccess
    onDialogAccept dlg $ \_ ->  do
        Just (_, (itm, _)) <- getSelected lst
        playVid itm
                         
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
            rd    <- prsVid <$> fetchHtml
            vdlst <- search rd <$> getArgs
            when (length vdlst < 1) $ error "empty list!"
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
        

            onKeyPressed lst $ \_ key _ -> case key of
                KEnter -> do
                    Just (_, (itm, _)) <- getSelected lst
                    fex itm >>= \case 
                        True  -> chgdl
                        False -> playVid itm
                    return True
                _      -> return False

            forkIO $ writeVid vdlst
            return ()
        return ()

    runUi c $ defaultContext {normalAttr = white `on` black, 
                              focusAttr  = black `on` blue
                             }
    where fex itm = downloaded $ vidName itm
          tryExit _ key _ = case key of
            KChar 'q' -> exitSuccess
            _         -> return False


crtInfPg :: Video -> T.Text
crtInfPg vid = T.unlines ["", "",
                          "File name: " `app` (spl . vidName) vid,
                          "File size: " `app` vidSize vid,
                          "File date: " `app` vidDate vid,
                          "File link: " `app` (spl . vidLink) vid
                         ]
    where app = T.append
          spl = T.intercalate "\n" . T.chunksOf 60 


