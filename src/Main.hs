{-# LANGUAGE OverloadedStrings #-}

import FetchHtml
import PrsVid
import Beaut
import Plain
import PlayVid
import Search
import Getconfig
import Chkcfg

import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import System.Exit
import Graphics.Vty
import System.Environment
import System.Directory
import Data.Array
import Control.Monad
import Control.Applicative
import System.IO
import qualified Data.Text.IO as TI

main = do
    chkcfg

    rd    <- prsVid <$> fetchHtml
    vdlst <- search rd <$> getArgs

    when (length vdlst < 1) $ error "empty list!"

    ------define widgets-----------
    (dlg, dfcg) <- flip newDialog "File Exists!" =<< plainText "redownload it?"
    lst <- newTextList (map beaut vdlst) 3

    -----preprocess fileExist-----
    locdir <- getLocaldir
    fex <- listArray (0, length vdlst - 1) <$> mapM (\(n, _, _) -> doesFileExist $ T.unpack $ T.append locdir n) vdlst
    let schg sle = case sle of 
                SelectionOn id _ _ -> if fex!id then setFocusAttribute lst (black `on` red) 
                                                else setFocusAttribute lst (black `on` green)
                _                  -> return ()
    let deftAttr ls = if fex!0 then setFocusAttribute ls (black `on` red)
                               else setFocusAttribute ls (black `on` green)

    ------generate ui----------
    lui <- centered =<< hFixed 80 lst
    dui <- centered =<< hFixed 30 (dialogWidget dlg)

    -------define focus group-----
    lfg <- newFocusGroup
    lfg  `onKeyPressed` (\_ key _ -> if key == KChar 'q' then exitSuccess else return False)
    dfcg `onKeyPressed` (\_ key _ -> if key == KChar 'q' then exitSuccess else return False)
    setFocusGroupNextKey dfcg KRight []
    setFocusGroupPrevKey dfcg KLeft  []
    addToFocusGroup lfg lst
    -------define Collections------
    c <- newCollection
    chgls <- addToCollection c lui lfg
    chgdl <- addToCollection c dui dfcg
    
    ---------create infor screen-----------
    ifsfg <- newFocusGroup
    chgif <- forM vdlst (\vid -> do
                            infw <- plainText $ crtInfPg vid
                            bnfw <- centered infw
                            addToFocusGroup ifsfg bnfw
                            addToCollection c bnfw ifsfg)
    ifsfg `onKeyPressed` (\_ key _ -> if key == KChar 'q' then exitSuccess else return False)
    ifsfg `onKeyPressed` (\_ key _ -> if key == KLeft     then chgls >> return True  else return False)
    ------define activate process--------
    
    let chgtx tx = do
            clearList lst
            forM_ tx $ \n -> addToList lst n =<< plainText n

    let lnch = do
          res <- getSelected lst
          case res of
            Nothing -> return False
            Just (ind, _) -> if fex!ind then chgdl                >> return False 
                                        else playVid (vdlst!!ind) >> return True
    dlg `onDialogCancel` const exitSuccess
    dlg `onDialogAccept` (\_ ->  do
                             res <- getSelected lst
                             case res of
                               Nothing       -> return ()
                               Just (ind, _) -> playVid (vdlst!!ind) >> exitSuccess
                         )
    -------------------
    let showInfor = do
          now <- getSelected lst
          case now of
            Nothing -> return ()
            Just (ind, _) -> chgif!!ind >> return () -- chgif is a list of IO action chg to inf
    let retnToLst = chgls
    ---------lst--------
    lst `onKeyPressed` (\_ key _ -> if key == KEnter then lnch >>= (\c -> if c then exitSuccess
                                                                               else return True)
                                                      else return False)

    lst `onKeyPressed` (\_ key _ -> if key == KRight then showInfor >> return True else return False)

    lst `onSelectionChange` schg
    deftAttr lst

    --------main loop---------
    runUi c $ defaultContext {normalAttr = white `on` black, 
                              focusAttr  = black `on` green
                             }


crtInfPg :: (T.Text, T.Text, T.Text) -> T.Text
crtInfPg (name, link, size) = T.unlines ["", "",
                                         ("FileName: " `T.append` name),
                                         ("FileLink: " `T.append` link),
                                         ("FileSize: " `T.append` size)
                                        ]



