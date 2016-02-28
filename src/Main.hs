{-# LANGUAGE OverloadedStrings #-}
module Main
where

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

drawUI :: L.List Entry -> [Widget]
drawUI l = [ui]
    where
        cur = case (L.listSelectedElement l) of
                Nothing -> str "-"
                Just (i, _) -> str (show (i + 1))
        ui = C.vCenter box
        box = B.border . hLimit 25 . vLimit 25 $ L.renderList l listDrawElement
        listDrawElement False a = str . T.unpack $ visibleName a 
        listDrawElement True a = str . T.unpack $ visibleName a

main = do
    -- mapM_ print =<< fetchRoot
    flst <- fetchRoot
    let
        initialState :: L.List Entry
        initialState = L.list (T.Name "root") (V.fromList flst) 1
        theApp =
            M.App { M.appDraw = drawUI
                  , M.appChooseCursor = M.neverShowCursor
                  , M.appHandleEvent = appEvent
                  , M.appStartEvent = return
                  , M.appAttrMap = const theMap 
                  , M.appLiftVtyEvent = id
                  }
        appEvent :: L.List Entry -> V.Event -> T.EventM (T.Next (L.List Entry))
        appEvent l e = case e of
            V.EvKey V.KEsc [] -> M.halt l
            ev -> M.continue =<< T.handleEvent ev l 
        theMap = A.attrMap V.defAttr [ (L.listAttr, V.white `on` V.black)
                                     , (L.listSelectedAttr, V.black `on` V.cyan)
                                     ]
    M.defaultMain theApp initialState
    return ()

