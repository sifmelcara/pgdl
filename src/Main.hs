{-# LANGUAGE OverloadedStrings #-}
module Main
where

import Debug.Trace
import Fetcher
import qualified Data.Text as T
import Networking
import Control.Monad

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
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

import Text.HTML.DirectoryListing.Type

drawUI :: LState -> [Widget]
drawUI (LState _ l) = [ui]
    where
        cur = case (L.listSelectedElement l) of
                Nothing -> str "-"
                Just (i, _) -> str (show (i + 1))
        ui = C.vCenter box
        box = B.border . hLimit 25 . vLimit 25 $ L.renderList l listDrawElement
        listDrawElement _ (Directory a _) = str . T.unpack $ visibleName a 
        listDrawElement _ (File a) = str . T.unpack $ visibleName a 

data LState = LState LState (L.List DNode) | TerminalLState

main = do
    trace "starting fetch in main" $ return ()
    dNodes <- fetch
    trace "finish fetch in main" $ return ()
    let
        initialState :: LState
        initialState = LState TerminalLState lst
            where
            lst = L.list (T.Name "root") (V.fromList dNodes) 1
        theApp =
            M.App { M.appDraw = drawUI
                  , M.appChooseCursor = M.neverShowCursor
                  , M.appHandleEvent = appEvent
                  , M.appStartEvent = return
                  , M.appAttrMap = const theMap 
                  , M.appLiftVtyEvent = id
                  }
        appEvent :: LState -> V.Event -> T.EventM (T.Next LState)
        appEvent ls@(LState father lst) e = case e of
            V.EvKey V.KEsc [] -> M.halt ls
            V.EvKey V.KEnter [] -> case child of
                                    Directory entry dns -> M.continue $ LState ls $ L.list (T.Name "asdf") (V.fromList dns) 1
                                    _ -> M.continue ls
            ev -> M.continue =<< (LState father <$> (T.handleEvent ev lst))
            where
            Just (_, child) = L.listSelectedElement lst
        theMap = A.attrMap V.defAttr [ (L.listAttr, V.white `on` V.black)
                                     , (L.listSelectedAttr, V.black `on` V.cyan)
                                     ]
    M.defaultMain theApp initialState
    return ()

