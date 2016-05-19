{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main
where

import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Text (Text)
import Data.Maybe
import DownloadInterface
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import System.FilePath
import System.Environment
import System.IO
import Text.HTML.DirectoryListing.Type

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import Brick.Types (Widget)
import Brick.Widgets.Core
import Brick.Util (on)

import EntryAttrViewer
import Utils
import qualified Configure as Conf
import Cache
import Types
import Local
import Networking
import qualified Utils as U

-- |                    father            contents                 
data MainState = LState (Maybe MainState) (L.List DNode)
               | SearchState MainState (L.List DNode) E.Editor


main :: IO ()
main = do
    let askPassword = do
            putStr "please input password: "
            hFlush stdout
            hSetEcho stdin False
            pass <- getLine
            hSetEcho stdin True
            putChar '\n'
            return $ T.pack pass
    (dNodes, nr) <- getArgs >>= \case
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
                                                                        pass <- askPassword
                                                                        return (ru, Just (user, pass))
                                                                    Just pass -> return (ru, Just (user, pass))
                                                [r] -> return (T.pack r, Nothing)
                                                _ -> error "too many arguments."
                            putStrLn "loading webpage..."
                            putStrLn "(you can use 'pgdl --offline' to browse the webpage you load last time)"
                            nr <- genNetworkResource rootUrl up
                            dNodes <- fetch nr 
                            return (dNodes, nr)
    let
        initialState :: MainState
        initialState = LState Nothing lst
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
            V.EvKey V.KEnter mod -> case L.listSelectedElement lst of
                                    Nothing -> M.continue ls
                                    Just (rowNum, child) -> case child of
                                        Directory entry dnsOp -> do
                                            dns <- liftIO dnsOp -- grab the subdirectory
                                            M.continue $ LState (Just ls) $ L.list (T.Name "root") (V.fromList dns) 3
                                        File entry url False -> do
                                            let fn = decodedName entry
                                            path <- liftIO $ Conf.getLocaldir >>= \case
                                                                Nothing -> return fn 
                                                                Just pre -> return $ T.pack (T.unpack pre </> T.unpack fn)
                                            let dui = downloadInterface $ DownloadSettings { networkResource = nr
                                                                                           , relativeUrl = url
                                                                                           , localStoragePath = path
                                                                                           , justOpen = False
                                                                                           , continueDownload = False
                                                                                           }
                                                -- | construct a newList, modify the downloaded
                                                -- file's *downloaded state* to True.
                                                -- Maybe this is not a good approach?
                                                newList = L.listMoveTo rowNum . L.listInsert rowNum (File entry url True) . L.listRemove rowNum $ lst
                                            M.suspendAndResume $ dui >> return (LState father newList)
                                        File entry url True -> do -- already downloaded file
                                            let fn = decodedName entry
                                            path <- liftIO $ Conf.getLocaldir >>= \case
                                                                Nothing -> return fn 
                                                                Just pre -> return $ T.pack (T.unpack pre </> T.unpack fn)
                                            let dui = downloadInterface $ DownloadSettings { networkResource = nr
                                                                                           , relativeUrl = url
                                                                                           , localStoragePath = path
                                                                                           , justOpen = mod /= [V.MMeta]
                                                                                           , continueDownload = mod == [V.MMeta] 
                                                                                           }
                                            --                                    ^ not good, unsafe
                                            M.suspendAndResume $ dui >> return ls
            V.EvKey V.KLeft [] -> M.continue $ fromMaybe ls father
            V.EvKey V.KRight [] -> case L.listSelectedElement lst of
                                    Nothing -> M.continue ls
                                    Just (_, sel) -> M.suspendAndResume $ entryAttrViewer sel >> return ls
            V.EvKey (V.KChar '/') [] -> M.continue $ SearchState ls lst (E.editor "searchBar" (str.unlines) (Just 1) "")
            V.EvKey (V.KChar 'd') [] -> case L.listSelectedElement lst of
                                            Nothing -> M.continue ls
                                            Just (rowNum, child) -> case child of
                                                Directory _ _ -> M.continue ls
                                                File entry url False -> M.continue ls
                                                File entry url True -> do -- already downloaded file
                                                    let fn = decodedName entry
                                                    path <- liftIO $ Conf.getLocaldir >>= \case
                                                                        Nothing -> return fn 
                                                                        Just pre -> return $ T.pack (T.unpack pre </> T.unpack fn)
                                                    liftIO $ deleteFile path
                                                    -- | very bad approach. File deletion may fail.
                                                    let newList = L.listMoveTo rowNum . L.listInsert rowNum (File entry url False) . L.listRemove rowNum $ lst
                                                    M.continue $ LState father newList
                                                    
            --                                                      ^ current list which reactively change with editor
            ev -> M.continue =<< (LState father <$> T.handleEvent ev lst)
        appEvent ss@(SearchState ms@(LState _ origLst) lst ed) e = case e of
            V.EvKey V.KEsc [] -> M.halt ss
            V.EvKey V.KEnter [] -> case E.getEditContents ed of
                [""] -> M.continue ms -- ^ do nothing if the editor is empty
                _ -> M.continue $ LState (Just ms) lst
            ev -> do
                newEd <- T.handleEvent ev ed
                -- | update the list, lst
                let 
                    linesToALine [l] = l
                    linesToALine _ = error "not one line of words in the search bar, why?"
                    -- | Why does L.listReplace require an Eq instance of Vector elements...?
                    applyFilter kw lst = replaceList newElms lst
                        where
                        newElms = V.filter (\dn -> kw `isKeyWordOf` getDNText dn) $ L.listElements lst
                        -- | sometime this will crash? how?
                        replaceList es l = l {L.listElements = es, L.listSelected = Just 0}
                    getDNText (Directory e _) = decodedName e
                    getDNText (File e _ _) = decodedName e 
                    isKeyWordOf t1 t2 = T.toCaseFold t1 `T.isInfixOf` T.toCaseFold t2
                M.continue $ SearchState ms (applyFilter (T.pack . linesToALine $ E.getEditContents newEd) origLst) newEd
        appEvent SearchState {} _ = error "unexpected prev state in SearchState."
        theMap = A.attrMap V.defAttr [ (L.listAttr, V.white `on` V.black)
                                     , ("directory", V.black `on` V.magenta)
                                     , ("file", V.black `on` V.cyan)
                                     , ("downloaded file", V.black `on` V.red)
                                     , ("statusBar", V.black `on` V.green)
                                     , ("searchBar", V.black `on` V.blue)
                                     ]
    M.defaultMain theApp initialState
    return ()


-- | use cropping to draw UI in the future?
drawUI :: MainState -> [Widget]
drawUI mainState = case mainState of
        (LState _ l) -> [ C.hCenter . hLimit U.terminalWidth $
                          vBox [entryList l, statusBar l]
                        ]
        (SearchState _ l e) -> [ C.hCenter . hLimit U.terminalWidth $
                                 vBox [entryList l, searchBar e]
                               ]
    where
    -- fixme (brick bug?): the vertical size of the list 
    -- is somewhat strange when the hroizontal size limit
    -- is not the multiple of its element size (it is 3)
    -- This strange behavior do not occur in vty-ui
    entryList lst = L.renderList lst listDrawElement
    listDrawElement False (Directory a _) = C.hCenter . txt . mid . stripWidth $ decodedName a 
    listDrawElement False (File a _ _) = C.hCenter . txt . mid . stripWidth $ decodedName a 
    listDrawElement True d@(Directory _ _) = withAttr "directory" $ listDrawElement False d
    listDrawElement True f@(File _ _ False) = withAttr "file" $ listDrawElement False f
    listDrawElement True f@(File _ _ True) = withAttr "downloaded file" $ listDrawElement False f
    mid s = T.unlines ["", s, ""]
    stripWidth :: Text -> Text
    stripWidth t = case U.cutTextByDisplayLength (U.terminalWidth-5) t of
                    [] -> ""
                    [singleLine] -> singleLine
                    (x:_) -> x `T.append` "..."

    searchBar ed = forceAttr "searchBar" $ hBox [txt "search: ", E.renderEditor ed]

    statusBar lst = withAttr "statusBar" . str . expand $ info lst
    expand s = s ++ replicate 88 ' '
    info lst = case L.listSelectedElement lst of
            Nothing -> "Nothing selected by user"
            Just (_, sel) -> "  " ++ show (lastModified entry) ++ "    " ++ maybe "Nothing" friendlySize (fileSize entry)
                where
                entry = case sel of
                            Directory e _ -> e
                            File e _ _ -> e

