{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import FetchHtml
import PrsVid
import Beaut
import PlayVid
import Search
import Getconfig
import Chkcfg
import Video

import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import System.FilePath
import System.Exit
import Graphics.Vty
import System.Environment
import System.Directory
import Control.Monad
import Control.Applicative
import System.IO
import Control.Concurrent

main = do
    chkcfg

    let testv = Video {vidName = "ffff", vidLink = "==++--", vidSize="",vidDate="44"}
    ------define widgets-----------
    (dlg, dfcg) <- flip newDialog "File Exists!" =<< plainText "redownload it?"
    lst <- newList 3 
    addToList lst testv =<< (plainText $ beaut testv)

    -----preprocess fileExist-----
    locdir <- getLocaldir
    ------generate ui----------
    lui <- centered =<< hFixed 80 lst
    dui <- centered =<< hFixed 30 (dialogWidget dlg)

    -------define focus group-----
    lfg <- newFocusGroup
    setFocusGroupNextKey dfcg KRight []
    setFocusGroupPrevKey dfcg KLeft  []
    addToFocusGroup lfg lst

    -------define Collections------
    c <- newCollection
    chgls <- addToCollection c lui lfg
    chgdl <- addToCollection c dui dfcg
    
    ---------create infor screen-----------

    -------exit when q is pressed-------
    lfg   `onKeyPressed` (\_ key _ -> if key == KChar 'q' then exitSuccess else return False)
    dfcg  `onKeyPressed` (\_ key _ -> if key == KChar 'q' then exitSuccess else return False)

    ------define activate process--------
    
                                            
    dlg `onDialogCancel` const exitSuccess
    dlg `onDialogAccept` (\_ ->  do
                             Just (_, (itm, _)) <- getSelected lst
                             playVid itm
                         )

    ---------lst--------
    lst `onKeyPressed` (\_ key _ -> case key of
                            KEnter -> do
                                Just (_, (itm, _)) <- getSelected lst
                                fex itm >>= \case 
                                    True  -> chgdl
                                    False -> playVid itm
                                return True
                            _      -> return False
                       )
    ifsfg <- newFocusGroup
    ifsfg `onKeyPressed` (\_ key _ -> if key == KLeft then chgls >> return True else return False)
    ifsfg `onKeyPressed` (\_ key _ -> if key == KChar 'q' then exitSuccess else return False)

    lst `onKeyPressed` (\_ key _ -> case key of
                            KRight -> do
                                Just (_, (itm, _)) <- getSelected lst
                                inf <- centered =<< (plainText $ crtInfPg itm)
                                addToFocusGroup ifsfg inf
                                chgif <- addToCollection c inf ifsfg
                                chgif
                                return True
                            _ -> return False)

    onSelectionChange lst $ \sle -> case sle of
        SelectionOn _ itm _ -> fex itm >>= \case 
                True  -> setFocusAttribute lst (black `on` red) 
                False -> setFocusAttribute lst (black `on` green)
        _                   -> return ()


    ------evaluate real list--------
    schedule $ do
        forkIO $ do
            rd    <- prsVid <$> fetchHtml
            newVdlst <- search rd <$> getArgs
            when (length newVdlst < 1) $ error "empty list!"
            clearList lst
            forM_ newVdlst $ \v -> do
                addToList lst v =<< plainText (beaut v)
        return ()

    runUi c $ defaultContext {normalAttr = white `on` black, 
                              focusAttr  = black `on` green
                             }

fex itm = do
    --Just (_, (itm, _)) <- getSelected lst
    locd <- fmap T.unpack getLocaldir
    doesFileExist $ locd </> (T.unpack . vidName $ itm)

crtInfPg :: Video -> T.Text
crtInfPg vid = T.unlines ["", "",
                          "File name: " `app` (spl . vidName) vid,
                          "File size: " `app` vidSize vid,
                          "File date: " `app` vidDate vid,
                          "File link: " `app` (spl . vidLink) vid
                         ]
               where app = T.append
                     spl = T.intercalate "\n" . T.chunksOf 60 


