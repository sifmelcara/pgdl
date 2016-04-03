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
import qualified Brick.Widgets.Edit as E
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
  , vBox, hBox
  , withAttr
  , forceAttr
  , txt
  )
import Brick.Util (fg, on)

import Text.HTML.DirectoryListing.Type
import Data.Maybe
import EntryAttrViewer
import Utils
import Configure
import System.Environment
import Cache
import Types

-- | use cropping to draw UI in the future?
drawUI :: MainState -> [Widget]
drawUI mainState = case mainState of
        (LState _ l) -> [C.hCenter . hLimit 80 $ vBox [entryList l, statusBar l]]
        (SearchState _ l e) -> [C.hCenter . hLimit 80 $ vBox [entryList l, searchBar e]]
    where
    -- note: the vertical size of the list is somewhat strange when the hroizontal size limit
    -- is not the multiple of its element size (it is 3)
    entryList lst = L.renderList lst listDrawElement
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

    searchBar ed = forceAttr "searchBar" $ hBox [txt "search: ", E.renderEditor ed]

    statusBar lst = withAttr "statusBar" . str . expand $ info lst
    expand s = s ++ replicate 88 ' '
    info lst = case L.listSelectedElement lst of
            Nothing -> "Nothing selected by user"
            Just (_, sel) -> "  " ++ show (lastModified entry) ++ "    " ++ maybe "Nothing" friendlySize (fileSize entry)
                where
                entry = case sel of
                            Directory entry _ -> entry
                            File entry _ -> entry
    

-- |                 father  contents                 
data MainState = LState MainState (L.List DNode)
               | SearchState MainState (L.List DNode) E.Editor
               | SortSelectionState MainState

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
        initialState :: MainState
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
        appEvent :: MainState -> V.Event -> T.EventM (T.Next MainState)
        appEvent ls@(LState father lst) e = case e of
            V.EvKey V.KEsc [] -> M.halt ls
            V.EvKey (V.KChar 'q') [] -> M.halt ls
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
                                    Just (_, sel) -> M.suspendAndResume $ entryAttrViewer sel >> return ls
            V.EvKey (V.KChar '/') [] -> M.continue $ SearchState ls lst (E.editor "searchBar" (str.unlines) (Just 1) "")
            --                                                      ^ current list which reactively change with editor
            ev -> M.continue =<< (LState father <$> (T.handleEvent ev lst))
        appEvent ss@(SearchState ms@(LState _ origLst) lst ed) e = case e of
            V.EvKey V.KEsc [] -> M.halt ss
            V.EvKey V.KEnter [] -> M.continue $ LState ms lst
            ev@(V.EvKey (V.KChar _) []) -> do
                newEd <- T.handleEvent ev ed
                -- | update the list, lst
                let 
                    linesToALine [l] = l
                    linesToALine _ = error "not one line of words in the search bar, why?"
                    -- | Why does L.listReplace require an Eq instance of Vector elements...?
                    applyFilter kw lst = replaceList newElms lst
                        where
                        newElms = V.filter (\dn -> kw `isKeyWordOf` (getDNText dn)) $ L.listElements lst
                        -- | sometime this will crash? how?
                        replaceList es l = l {L.listElements = es, L.listSelected = Just 0}
                    getDNText (Directory e _) = decodedName e
                    getDNText (File e _) = decodedName e 
                    isKeyWordOf t1 t2 = T.toCaseFold t1 `T.isInfixOf` T.toCaseFold t2
                M.continue $ SearchState ms (applyFilter (T.pack . linesToALine $ E.getEditContents newEd) origLst) newEd
            ev -> M.continue ss
        theMap = A.attrMap V.defAttr [ (L.listAttr, V.white `on` V.black)
--                                     , (L.listSelectedAttr, V.black `on` V.cyan)
                                     , ("directory", V.black `on` V.magenta)
                                     , ("file", V.black `on` V.cyan)
                                     , ("statusBar", V.black `on` V.green)
                                     , ("searchBar", V.black `on` V.blue)
                                     ]
    M.defaultMain theApp initialState
    return ()

