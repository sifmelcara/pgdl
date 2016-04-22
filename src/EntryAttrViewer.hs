{-# LANGUAGE OverloadedStrings #-}
module EntryAttrViewer (entryAttrViewer)
where

import Debug.Trace
import qualified Data.Text as T
import Data.Text (Text)
import Networking
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import qualified Control.Concurrent.Chan as C

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.ProgressBar as P
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as C
import qualified Brick.AttrMap as A
import qualified Data.Vector as V
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)

import Control.Concurrent

import Control.Monad.Trans.Resource 
import Data.Conduit
import Control.Monad.IO.Class

import Fetcher
import Types
import Text.HTML.DirectoryListing.Type

import qualified Utils as U

entryAttrViewer :: DNode -> IO ()
-- | Temporary use File to process Directory...
entryAttrViewer (Directory entry _) = entryAttrViewer (File entry "directory no link.")
entryAttrViewer (File entry url) = do
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
           zipWith (\describ str -> T.intercalate "\n" . 
                                    U.cutTextByDisplayLength U.terminalWidth $
                                    describ `T.append` ": " `T.append` str
                   )
           [ "visible name" 
           , "decoded name" 
           , "url" 
           , "file size" 
           ]
           [ visibleName entry
           , decodedName entry
           , url
           , (T.pack . show $ fileSize entry) `T.append` " bytes"
           ]

