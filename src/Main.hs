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

    let vdlst = return $ Video {vidName = "ffff", vidLink = "==++--", vidSize="",vidDate="44"}
    ------define widgets-----------
    (dlg, dfcg) <- flip newDialog "File Exists!" =<< plainText "redownload it?"
    lst <- newTextList (map beaut vdlst) 3

    -----preprocess fileExist-----
    locdir <- getLocaldir
    let schg sle = case sle of 
                SelectionOn id _ _ -> fex lst id >>= \case 
                                        True  -> setFocusAttribute lst (black `on` red) 
                                        False -> setFocusAttribute lst (black `on` green)
                _                  -> return ()
    let deftAttr ls = fex lst 0 >>= \case 
                                    True  -> setFocusAttribute ls (black `on` red)
                                    False -> setFocusAttribute ls (black `on` green)

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
    ifsfg <- newFocusGroup
    chgif <- forM vdlst (\vid -> do
                            infw <- plainText $ crtInfPg vid
                            bnfw <- centered infw
                            addToFocusGroup ifsfg bnfw
                            addToCollection c bnfw ifsfg)
    ifsfg `onKeyPressed` (\_ key _ -> if key == KLeft     then chgls >> return True  else return False)

    -------exit when q is pressed-------
    lfg   `onKeyPressed` (\_ key _ -> if key == KChar 'q' then exitSuccess else return False)
    dfcg  `onKeyPressed` (\_ key _ -> if key == KChar 'q' then exitSuccess else return False)
    ifsfg `onKeyPressed` (\_ key _ -> if key == KChar 'q' then exitSuccess else return False)

    ------define activate process--------
    
    let lnch = do
          res <- getSelected lst
          case res of
            Nothing -> return False
            Just (ind, _) -> fex lst ind >>= \case
                                        True  -> chgdl >> return False 
                                        False -> playVid (vdlst!!ind) >> return True
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

    ------evaluate real list--------
    schedule $ do
        forkIO $ do
            rd    <- prsVid <$> fetchHtml
            newVdlst <- search rd <$> getArgs
            when (length newVdlst < 1) $ error "empty list!"
            clearList lst
            forM_ newVdlst $ \v -> do
                addToList lst (vidName v) =<< plainText (beaut v)
        return ()

    runUi c $ defaultContext {normalAttr = white `on` black, 
                              focusAttr  = black `on` green
                             }

fex lst ind = do
    Just (str, _) <- getListItem lst ind
    locd <- fmap T.unpack getLocaldir
    doesFileExist $ locd </> (T.unpack str)

crtInfPg :: Video -> T.Text
crtInfPg vid = T.unlines ["", "",
                          "File name: " `app` (spl . vidName) vid,
                          "File size: " `app` vidSize vid,
                          "File date: " `app` vidDate vid,
                          "File link: " `app` (spl . vidLink) vid
                         ]
               where app = T.append
                     spl = T.intercalate "\n" . T.chunksOf 60 


