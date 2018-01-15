{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase#-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module DownloadInterface (downloadInterface, DownloadSettings(..))
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import Data.Conduit.Binary
import Data.List
import Data.Monoid
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Applicative
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import System.Posix.Files
import System.Process
import qualified System.IO as IO
import Distribution.System
import qualified Control.Concurrent.Chan as C
import qualified Brick.BChan as BC

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.ProgressBar as P
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Types (Widget)
import Brick.Widgets.Core
import Brick.Util (on)

import Networking

data DownloadSettings =
    DownloadSettings { networkResource :: NetworkResource
                     , relativeUrl :: Text
                     , localStoragePath :: Text
                     -- ^ file name included
                     , justOpen :: Bool
                     -- ^ when True, don't use any network resource
                     , continueDownload :: Bool
                     -- ^ when True, continue the download progress (use "Range" in http header)
                     -- else redownload the whole file
                     }

--                                 bytes already downloaded
data DownloadState = DownloadState Integer | FinishedState
                   | UserInput DownloadState (E.Editor String String)
                   --          ^ download progress

data DownloadEvent = UpdateFinishedSize Integer
                   | DownloadFinish

downloadInterface :: DownloadSettings -> IO ()
downloadInterface dSettings = do
    eventChan <- BC.newBChan 100000000 -- select a big enough number
    unless (justOpen dSettings) . void . forkIO $
        download dSettings (BC.writeBChan eventChan)
    progressBarTotSize <- if justOpen dSettings
                          then getLocalFileSize (T.unpack $ localStoragePath dSettings) >>= \case
                                Nothing -> error "file do not exist."
                                Just s -> return s
                          else getContentLength dSettings
    let
        initialState = if justOpen dSettings
                       then FinishedState
                       else DownloadState 0
        theApp =
            M.App { M.appDraw = drawUI
                  , M.appChooseCursor = M.neverShowCursor
                  , M.appHandleEvent = appEvent
                  , M.appStartEvent = return
                  , M.appAttrMap = const theMap
                  }
        appEvent :: DownloadState -> T.BrickEvent String DownloadEvent -> T.EventM String (T.Next DownloadState)
        appEvent ds@(DownloadState _) be = case be of
            T.VtyEvent e -> case e of
                V.EvKey V.KEsc [] -> M.halt ds
                V.EvKey (V.KChar 'q') _ -> M.halt ds
                V.EvKey (V.KChar 'o') [] -> M.continue $ UserInput ds (E.editor "command" (Just 1) "")
                V.EvKey V.KEnter [] -> do
                    liftIO $ localStoragePath dSettings `openBy` ""
                    M.continue ds
                _ -> M.continue ds
            T.AppEvent ae -> case ae of
                UpdateFinishedSize b -> M.continue . DownloadState $ b
                DownloadFinish -> M.continue FinishedState
        appEvent FinishedState be = case be of
            T.VtyEvent e -> case e of
                V.EvKey (V.KChar 'q') _ -> M.halt FinishedState
                V.EvKey (V.KChar 'o') [] -> M.continue $ UserInput FinishedState (E.editor "command" (Just 1) "")
                V.EvKey V.KEnter [] -> do
                    liftIO $ localStoragePath dSettings `openBy` ""
                    M.halt FinishedState
                    -- ^ file opened, we can quit download interface now
                _ -> M.continue FinishedState
            _ -> error "received non vty event after FinishedState was reached."
        appEvent (UserInput st ed) be = case be of
            T.VtyEvent e -> case e of
                V.EvKey V.KEsc [] -> M.continue st
                V.EvKey V.KEnter [] -> do
                    liftIO $ localStoragePath dSettings `openBy` concat (E.getEditContents ed)
                    case st of
                        DownloadState _ -> M.continue st
                        FinishedState -> M.halt FinishedState
                        _ -> error "unexpected state in UserInput state."
                ev -> do
                    newEd <- E.handleEditorEvent ev ed
                    M.continue $ UserInput st newEd
            T.AppEvent de -> case de of
                UpdateFinishedSize b -> M.continue (UserInput (DownloadState b) ed)
                DownloadFinish -> M.continue (UserInput FinishedState ed)
        appEvent _ _ = error "unknown event received in event loop. (DL interface)"
        theMap = A.attrMap V.defAttr [ (P.progressCompleteAttr, V.black `on` V.cyan)
                                     , (P.progressIncompleteAttr, V.black `on` V.white)
                                     , ("input box", V.black `on` V.blue)
                                     ]
        drawUI :: DownloadState -> [Widget String]
        drawUI (DownloadState bytes) = [vBox [bar, note]]
            where
            bar = C.vCenter . C.hCenter $ P.progressBar Nothing (fromIntegral bytes / fromIntegral progressBarTotSize)
            note = C.vCenter . C.hCenter . str $ unlines [ "press Enter to open the file (even if its download has not finished)"
                                                         , "press 'o' to specify which program should be used to open the file."
                                                         ]
        drawUI FinishedState = [ui]
            where
            ui = C.vCenter . C.hCenter . str $ unlines [ "The file is ready for open."
                                                       , "press Enter to open the file, or press 'q' to return to file listing"
                                                       , "press 'o' to specify which program should be used to open the file."
                                                       ]
        drawUI (UserInput (UserInput _ _) _) = error "unexpected state in UserInput state."
        drawUI (UserInput s ed) = (padTop (T.Pad 1) . vLimit 5 $ ask) : drawUI s
            where
            ask = C.vCenter . C.hCenter .
                  hLimit 50 . vLimit 5 .
                  B.borderWithLabel (str "please input a program name") .
                  forceAttr "input box" .
                  hLimit 40 $
                  E.renderEditor (str . unlines) True ed
    M.customMain (V.mkVty mempty) (Just eventChan) theApp initialState
    return ()

openBy :: Text -> String -> IO ()
openBy file "" = case buildOS of
                    OSX -> openBy file "open"
                    Linux -> openBy file "xdg-open"
                    _ -> error "don't know how to open file in this OS"
openBy file cmd = void $
    case buildOS of
        OSX   -> runCommand $ cmd ++ " " ++ addq filepath ++ " "
        Linux -> runCommand $ "nohup " ++ cmd ++ " " ++ addq filepath ++ " &>/dev/null &"
        _     -> error "don't know how to open file in this OS"
    where
    filepath = T.unpack file
    addq :: String -> String
    addq s = "\"" ++ s ++ "\""

getContentLength :: DownloadSettings -> IO Integer
getContentLength dSettings = do
    let (req, manager) = networkResource dSettings $ relativeUrl dSettings
    runResourceT $ do
        response <- http req manager
        let headers = responseHeaders response
        case find (\(hn, bs) -> hn == hContentLength) headers of
            Nothing -> error "no content-length presents in response header"
            Just (hn, bs) -> case BC.readInteger bs of
                Nothing -> error "no integer in content-length header"
                Just (sz, _) -> return sz

getLocalFileSize :: String -> IO (Maybe Integer)
getLocalFileSize path =
    fileExist path >>= \case
        False -> return Nothing
        True -> Just . fromIntegral . fileSize <$> getFileStatus path

download :: DownloadSettings ->
            (DownloadEvent -> IO ()) ->
            IO ()
download dSettings tell = do
    -- this implementation needs to be change
    -- since some filename contains characters that cannot be represented by String
    let
        filepath = T.unpack $ localStoragePath dSettings
        checkFile :: IO ()
        checkFile = forever $ do
            threadDelay 100000 -- 0.1 second
            getLocalFileSize filepath >>= \case
                Nothing -> return ()
                Just s -> tell . UpdateFinishedSize $ s
    checkerThreadID <- forkIO checkFile
    let (req, manager) = networkResource dSettings $ relativeUrl dSettings
    localFileSize <- getLocalFileSize (T.unpack $ localStoragePath dSettings) >>= \case
        Nothing -> return 0
        Just s -> return s
    remoteFileSize <- getContentLength dSettings
    let extraHeaders = [("Range", "bytes=" `BC.append` BC.pack (show localFileSize) `BC.append` "-")]
    let req' = if continueDownload dSettings
               then req { requestHeaders = extraHeaders ++ requestHeaders req }
               else req
    -- use http and ResumableSource in conduit for constant memory usage
    runResourceT $ do
        if continueDownload dSettings then
            when (localFileSize < remoteFileSize) $ do
                response <- http req' manager
                let body = responseBody response
                body $$+- sinkIOHandle (IO.openBinaryFile filepath IO.AppendMode)
        else do
            response <- http req' manager
            let body = responseBody response
            body $$+- sinkIOHandle (IO.openBinaryFile filepath IO.WriteMode)
        liftIO $ do
            killThread checkerThreadID
            tell DownloadFinish
    return ()

