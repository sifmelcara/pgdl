{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main
where

import Debug.Trace
import Fetcher
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString as B
import Data.Text.Encoding
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
  , txt
  )
import Brick.Util (fg, on)

import Text.HTML.DirectoryListing.Type
import Data.Maybe
import FileAttrViewer
import Utils
import Configure
import System.Environment
import Cache
import Types

drawUI :: LState -> [Widget]
drawUI (LState _ l) = [C.hCenter . hLimit 80 $ vBox [entryList, statusBar]]
    where
    -- note: the vertical size of the list is somewhat strange when the hroizontal size limit
    -- is not the multiple of its element size (it is 3)
    entryList = L.renderList l listDrawElement
    listDrawElement False (Directory a _) = C.hCenter . txt . mid . strip80 $ decodedName a 
    listDrawElement False (File a _) = C.hCenter . txt . mid . strip80 $ decodedName a 
    listDrawElement True d@(Directory _ _) = withAttr "directory" $ listDrawElement False d
    listDrawElement True f@(File _ _) = withAttr "file" $ listDrawElement False f
    mid s = T.unlines $ ["", s, ""]
    strip80 :: Text -> Text
    strip80 t
        | displayLength t > 75 = takeDLength 75 t `T.append` "..."
        | otherwise = t
        where
        displayLength = sum . map charDisplayLen . T.unpack
        -- | charDisplayLen determine a unicode character is a wide character or not
        -- (a wide character occupy 2 space in the terminal)
        -- this method may seem unreliable, but have no better idea.
        charDisplayLen :: Char -> Int
        charDisplayLen c
            | (B.length . encodeUtf8 . T.pack $ [c]) > 1 = 2
            | otherwise = 1
        takeDLength len t = T.pack . map snd .
                            takeWhile ((<len).fst) . accumFst .
                            T.unpack $ t
            where
            accumFst = scanl (\(l, _) r -> (l + charDisplayLen r, r)) (0, ' ')


    statusBar = withAttr "statusBar" . str . expand $ info
    expand s = s ++ replicate 88 ' '
    info = case L.listSelectedElement l of
            Nothing -> "Nothing selected by user"
            Just (_, sel) -> "  " ++ show (lastModified entry) ++ "    " ++ maybe "Nothing" friendlySize (fileSize entry)
                where
                entry = case sel of
                            Directory entry _ -> entry
                            File entry _ -> entry
    

-- |                 father  contents
data LState = LState LState (L.List DNode)

main = do
    let askUserServpath = undefined
    rootUrl <- getArgs >>= \case
                [url] -> return . T.pack $ url
                _ -> getServpath >>= \case
                        Nothing -> askUserServpath
                        Just p -> return p
    dNodes <- do
        getArgs >>= \case
            ["--offline"] -> readCache >>= \case
                Nothing -> error "no offline data or data corrupted."
                Just dlst -> return dlst
            _ -> do
                putStrLn "loading webpage..."
                putStrLn "(you can use --offline to browse the webpages you load last time)"
                fetch rootUrl
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
                                        File entry url -> do
                                            let
                                                fp = decodedName entry
                                                dui = downloadInterface url fp (fromJust $ fileSize entry)
                                                --                             ^ this fromJust need to be eliminated
                                            M.suspendAndResume $ dui >> return ls
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

