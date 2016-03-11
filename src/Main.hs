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
import Data.Maybe
import FileAttrViewer

drawUI :: LState -> [Widget]
drawUI (LState _ l) = [C.hCenter . hLimit 60 $ vBox [entryList, statusBar]]
    where
    entryList = L.renderList l listDrawElement
    listDrawElement False (Directory a _) = C.hCenter . str . mid . T.unpack $ decodedName a 
    listDrawElement False (File a _) = C.hCenter . str . mid . T.unpack $ decodedName a 
    listDrawElement True d@(Directory _ _) = withAttr "directory" $ listDrawElement False d
    listDrawElement True f@(File _ _) = withAttr "file" $ listDrawElement False f
    mid :: String -> String
    mid s = unlines $ ["", s, ""]

    statusBar = withAttr "statusBar" . str . expand $ info
    expand s = s ++ replicate 88 ' '
    info = case L.listSelectedElement l of
            Nothing -> "Nothing selected by user"
            Just (_, sel) -> case sel of
                Directory entry _ -> "  " ++ show (lastModified entry) ++ "    "
                File entry _ -> "  " ++ show (lastModified entry) ++ "    " ++ show (fileSize entry)

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
            V.EvKey V.KEnter [] -> case L.listSelectedElement lst of
                                    Nothing -> M.continue $ ls
                                    Just (_, child) -> case child of
                                        Directory entry dnsOp -> do
                                            dns <- liftIO dnsOp -- grab the subdirectory
                                            M.continue $ LState ls $ L.list (T.Name "root") (V.fromList dns) 3
                                        File entry url -> M.suspendAndResume $ downloadInterface url (fromJust $ fileSize entry) >> return ls
                                        --                                                           ^ this fromJust need to be eliminated
            V.EvKey V.KLeft [] -> M.continue father
            V.EvKey V.KRight [] -> case L.listSelectedElement lst of
                                    Nothing -> M.continue $ ls
                                    Just (_, sel) -> case sel of
                                        File _ _ -> M.suspendAndResume $ fileAttrViewer sel >> return ls
                                        Directory entry dnsOp -> do
                                            dns <- liftIO dnsOp -- grab the subdirectory
                                            M.continue $ LState ls $ L.list (T.Name "root") (V.fromList dns) 3
            ev -> M.continue =<< (LState father <$> (T.handleEvent ev lst))
        theMap = A.attrMap V.defAttr [ (L.listAttr, V.white `on` V.black)
--                                     , (L.listSelectedAttr, V.black `on` V.cyan)
                                     , ("directory", V.black `on` V.magenta)
                                     , ("file", V.black `on` V.cyan)
                                     , ("statusBar", V.black `on` V.green)
                                     ]
    M.defaultMain theApp initialState
    return ()

