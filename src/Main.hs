{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main
where

import Paths_pgdl (version)
import Data.Version (showVersion)

import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Control.Monad.IO.Class
import Control.Applicative
import System.FilePath ((</>))
import System.Environment (getArgs)
import System.Directory (removeFile, doesFileExist)
import Text.HTML.DirectoryListing.Type

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import Brick.Widgets.Core
import Brick.Types (Widget)
import Brick.Util (on)

import DownloadInterface
import EntryAttrViewer
import Utils
import qualified Configure as Conf
import Cache
import Types
import Networking
import DList
import qualified Utils as U

data MainState = LState DList
               | SearchState DList (E.Editor String String)

main :: IO ()
main =
    getArgs >>= \case
        ["-v"] -> putStrLn $ "pgdl " ++ showVersion version
        ["--version"] -> putStrLn $ "pgdl " ++ showVersion version
        _ -> mainUI

mainUI :: IO ()
mainUI = do
    (dNodes, nr) <- initializeResource
    let
        initialState :: MainState
        initialState = LState $ newDList dNodes
        theApp =
            M.App { M.appDraw = drawUI
                  , M.appChooseCursor = M.neverShowCursor
                  , M.appHandleEvent = appEvent
                  , M.appStartEvent = return
                  , M.appAttrMap = const theMap
                  }
        appEvent :: MainState -> T.BrickEvent String e -> T.EventM String (T.Next MainState)
        appEvent ls@(LState dlst) (T.VtyEvent e) = case e of
            V.EvKey V.KEsc [] -> M.halt ls
            V.EvKey (V.KChar 'q') [] -> M.halt ls
            V.EvKey V.KEnter mdf -> case extractSelectedDNode dlst of
                Nothing -> M.continue ls
                Just dnode -> case dnode of
                    Directory entry openOp -> do
                        dns <- liftIO openOp -- grab the subdirectory
                        M.continue $ LState (pushDList dlst dns)
                    File entry url False -> do
                        let fn = decodedName entry
                        path <- liftIO $ Conf.getLocaldir >>= \case
                            Nothing -> return fn
                            Just pre -> return $ T.pack (T.unpack pre </> T.unpack fn)
                        let dui = downloadInterface DownloadSettings { networkResource = nr
                                                                     , relativeUrl = url
                                                                     , localStoragePath = path
                                                                     , justOpen = False
                                                                     , continueDownload = False
                                                                     }
                        M.suspendAndResume $ do
                            dui
                            ex <- doesFileExist (T.unpack path)
                            return $ LState (fromJust $ replaceSelectedDNode dlst (File entry url ex))
                        --                   ^ this fromJust is ok since we can be sure that
                        -- there has something been selected. However this need to perform refactor in the future
                    File entry url True -> do -- already downloaded file
                        let fn = decodedName entry
                        path <- liftIO $ Conf.getLocaldir >>= \case
                            Nothing -> return fn
                            Just pre -> return $ T.pack (T.unpack pre </> T.unpack fn)
                        let dui = downloadInterface DownloadSettings { networkResource = nr
                                                                     , relativeUrl = url
                                                                     , localStoragePath = path
                                                                     , justOpen = mdf /= [V.MMeta]
                                                                     , continueDownload = mdf == [V.MMeta]
                                                                     }
                        M.suspendAndResume $ dui >> return ls
            V.EvKey V.KLeft [] -> M.continue . LState $ popDList dlst
            V.EvKey V.KRight [] -> case extractSelectedDNode dlst of
                Nothing -> M.continue ls
                Just d -> M.suspendAndResume $ entryAttrViewer d >> return ls
            V.EvKey (V.KChar '/') [] -> M.continue $ SearchState (dupDList dlst) (E.editor "searchBar" (Just 1) "")
            V.EvKey (V.KChar 'd') [] -> case extractSelectedDNode dlst of
                Nothing -> M.continue ls
                Just dnode -> case dnode of
                    Directory _ _ -> M.continue ls
                    File entry url False -> M.continue ls
                    File entry url True -> do -- already downloaded file
                        let fn = decodedName entry
                        path <- liftIO $ Conf.getLocaldir >>= \case
                                            Nothing -> return $ T.unpack fn
                                            Just pre -> return $ T.unpack pre </> T.unpack fn
                        liftIO $ removeFile path
                        ex <- liftIO $ doesFileExist path
                        M.continue $ LState $ fromJust (replaceSelectedDNode dlst (File entry url ex))
                        --                    ^ this fromJust is ok since we can be sure that
                        -- there has something been selected. However this need to perform refactor in the future
            ev -> M.continue =<< do
                dlst' <- adjustCurrentBrickList dlst $ L.handleListEvent ev
                return $ LState dlst'
        appEvent ss@(SearchState dlst ed) (T.VtyEvent e) = case e of
            V.EvKey V.KEsc [] -> M.halt ss
            V.EvKey V.KEnter [] -> case E.getEditContents ed of
                [""] -> M.continue (LState $ popDList dlst)
                _ -> M.continue $ LState dlst
            ev -> do
                newEd <- E.handleEditorEvent ev ed
                let
                    linesToALine [l] = l
                    linesToALine _ = error "not one line of words in the search bar, why?"
                    keyword = T.pack . linesToALine $ E.getEditContents newEd
                    cond :: DNode -> Bool
                    cond (File entry _ _) = keyword `isKeyWordOf` decodedName entry
                    cond (Directory entry _) = keyword `isKeyWordOf` decodedName entry
                    isKeyWordOf a b = T.toCaseFold a `T.isInfixOf` T.toCaseFold b
                M.continue $ SearchState (filterDList dlst cond) newEd
        appEvent _ _ = error "unknown event received in event loop."
        theMap = A.attrMap V.defAttr [ (L.listAttr, V.white `on` V.black)
                                     , ("directory", V.black `on` V.magenta)
                                     , ("file", V.black `on` V.cyan)
                                     , ("downloaded file", V.black `on` V.yellow)
                                     , ("statusBar", V.black `on` V.green)
                                     , ("searchBar", V.black `on` V.blue)
                                     ]
    M.defaultMain theApp initialState
    return ()

-- | use cropping to draw UI in the future?
drawUI :: MainState -> [Widget String]
drawUI mainState = case mainState of
        (LState dlst) -> [ C.hCenter . hLimit U.terminalWidth $
                           vBox [entryList dlst, statusBar (extractSelectedDNode dlst)]
                         ]
        (SearchState dlst e) -> [ C.hCenter . hLimit U.terminalWidth $
                                  vBox [entryList dlst, searchBar e]
                                ]
    where
    entryList dlist = renderDList dlist $ \b d -> hBox $ listDrawElement b d
    listDrawElement :: Bool -> DNode -> [Widget String]
    listDrawElement sel dn = [ color (not sel) attrName . vLimit 3 . hLimit 1 $ fill ' '
                             , color sel attrName text
                             , color (not sel) attrName . vLimit 3 . hLimit 1 $ fill ' '
                             ]
        where
        attrName = case dn of
            Directory _ _ -> "directory"
            File _ _ False -> "file"
            File _ _ True -> "downloaded file"
        name = case dn of
            Directory a _ -> a
            File a _ _ -> a
        text = txt . placeTextIntoRectangle 3 (U.terminalWidth-2) . stripWidth $ decodedName name
        color True attr = withAttr attr
        color False _ = id
        stripWidth :: Text -> Text
        stripWidth t = case U.cutTextByDisplayLength (U.terminalWidth-7) t of
                        [] -> ""
                        [singleLine] -> singleLine
                        (x:_) -> x `T.append` "..."
    searchBar ed = forceAttr "searchBar" $ hBox [txt " search: ", E.renderEditor (str . unlines) True ed]
    statusBar = withAttr "statusBar" . str . expand . info
    info Nothing = "  Nothing selected by user"
    info (Just sel) = "  " ++ show (lastModified etr) ++ "    " ++ maybe "Directory" friendlySize (fileSize etr)
        where
        etr = entry sel
        entry (Directory e _) = e
        entry (File e _ _) = e
    expand s = s ++ replicate 88 ' '

initializeResource :: IO ([DNode], NetworkResource)
initializeResource =
    getArgs >>= \case
        ["--offline"] -> readCache >>= \case
            Nothing -> error "no offline data or data corrupted."
            Just dlst -> return (dlst, error "no network resource, offline mode.")
        online -> do
            (rootUrl, up) <- case online of
                [] -> Conf.getServpath >>= \case
                    Nothing -> error "example usage: pgdl https://www.kernel.org/pub/"
                    Just ru -> Conf.getUsername >>= \case
                                 Nothing -> return (ru, Nothing)
                                 Just user -> Conf.getPassword >>= \case
                                    Nothing -> do
                                        pass <- U.askPassword
                                        return (ru, Just (user, pass))
                                    Just pass -> return (ru, Just (user, pass))
                [r] -> return (T.pack r, Nothing)
                _ -> error "too many arguments."
            putStrLn "loading webpage..."
            putStrLn "(you can use 'pgdl --offline' to browse the webpage you load last time)"
            nr <- genNetworkResource rootUrl up
            dNodes <- fetch nr
            return (dNodes, nr)

