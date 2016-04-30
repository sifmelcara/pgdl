{-# LANGUAGE OverloadedStrings #-}

module EntryAttrViewer (entryAttrViewer)
where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.ProgressBar as P
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as C
import qualified Brick.AttrMap as A
import Brick.Types (Widget)
import Brick.Widgets.Core
import Brick.Util (on)

import Types
import Text.HTML.DirectoryListing.Type

import qualified Utils as U

entryAttrViewer :: DNode -> IO ()
-- | Temporary use File to process Directory...
entryAttrViewer (Directory entry _) = entryAttrViewer (File entry "directory no link." False)
entryAttrViewer (File entry url downloaded) = do
    let
        initialState :: () 
        initialState = ()
        theApp =
            M.App { M.appDraw = drawUI
                  , M.appChooseCursor = M.neverShowCursor
                  , M.appHandleEvent = appEvent
                  , M.appStartEvent = return
                  , M.appAttrMap = const theMap 
                  , M.appLiftVtyEvent = id
                  }
        appEvent :: () -> V.Event -> T.EventM (T.Next ())
        appEvent () e = case e of
            V.EvKey V.KEsc [] -> M.halt ()
            V.EvKey (V.KChar 'q') [] -> M.halt ()
            V.EvKey V.KLeft [] -> M.halt ()
            ev -> M.continue ()
        theMap = A.attrMap V.defAttr [ (P.progressCompleteAttr, V.black `on` V.cyan)
                                     , (P.progressIncompleteAttr, V.black `on` V.white)
                                     ]
        drawUI :: () -> [Widget]
        drawUI _ = [ui]
            where
            ui = C.vCenter . C.hCenter .
                 C.hLimit U.terminalWidth . C.txt $ info
    M.defaultMain theApp initialState
    return ()
    where
    info = T.unlines $
           zipWith (\describ s -> T.intercalate "\n" . 
                                  U.cutTextByDisplayLength U.terminalWidth $
                                  describ `T.append` ": " `T.append` s
                   )
           [ "visible name" 
           , "decoded name" 
           , "url" 
           , "file size" 
           , "downloaded"
           ]
           [ visibleName entry
           , decodedName entry
           , url
           , maybe "directory" (T.pack . U.friendlySize) . fileSize $ entry
           , if downloaded then "Yes" else "No"
           ]

