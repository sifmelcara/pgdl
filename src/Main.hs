{-# LANGUAGE OverloadedStrings #-}
module Main
where

import Debug.Trace
import Fetcher
import qualified Data.Text as T
import Networking
import DownloadInterface
import Control.Monad
import Control.Monad.IO.Class

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
        ui = C.vCenter . C.hCenter $ box
        box = B.border . hLimit 30 . vLimit 30 $ L.renderList l listDrawElement
        listDrawElement _ (Directory a _) = C.hCenter . str . mid . T.unpack $ visibleName a 
        listDrawElement _ (File a _) = C.hCenter . str . mid . T.unpack $ visibleName a 
        mid :: String -> String
        mid s = unlines $ ["", s, ""]

-- |                 father  contents
data LState = LState LState (L.List DNode)

main = do
    dNodes <- fetch
    let
        initialState :: LState
        initialState = LState initialState lst
            where
            lst = L.list (T.Name "root") (V.fromList dNodes) 3
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
                                    Directory entry dnsOp -> do
                                        dns <- liftIO dnsOp -- grab the subdirectory
                                        M.continue $ LState ls $ L.list (T.Name "root") (V.fromList dns) 3
                                    File entry url -> M.suspendAndResume $ downloadInterface url >> return ls
            V.EvKey V.KLeft [] -> M.continue father
            ev -> M.continue =<< (LState father <$> (T.handleEvent ev lst))
            where
            Just (_, child) = L.listSelectedElement lst
        theMap = A.attrMap V.defAttr [ (L.listAttr, V.white `on` V.black)
                                     , (L.listSelectedAttr, V.black `on` V.cyan)
                                     ]
    M.defaultMain theApp initialState
    return ()

