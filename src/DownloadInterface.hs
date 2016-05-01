{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase#-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module DownloadInterface (downloadInterface)
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Control.Monad
import Control.Monad.Trans.Resource 
import Control.Monad.IO.Class
import Control.Concurrent
import qualified Control.Concurrent.Chan as C
import Network.HTTP.Conduit
import System.Posix.Files
import System.Process
import Distribution.System

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

import Configure

--                                 bytes already downloaded
data DownloadState = DownloadState Integer | FinishedState
                   | UserInput DownloadState E.Editor
                   --          ^ download progress

data DEvent = VtyEvent V.Event
            | UpdateFinishedSize Integer
            | DownloadFinish

-- | Maybe we should try to get file size by other method,
-- that would be more accurate and reliable.
downloadInterface :: Text -> -- ^ url
                     Text -> -- ^ file path (may be a absolute path)
                     Integer -> -- ^ filesize in bytes
                     Bool -> -- ^ is the download already finished?
                     IO ()
downloadInterface url filepath filesize alreadyFinished = do
    eventChan <- C.newChan 
    unless alreadyFinished . void . forkIO $ download url filepath (C.writeChan eventChan)
    let
        initialState :: DownloadState 
        initialState = if alreadyFinished
                       then FinishedState
                       else DownloadState 0
        theApp =
            M.App { M.appDraw = drawUI
                  , M.appChooseCursor = M.neverShowCursor
                  , M.appHandleEvent = appEvent
                  , M.appStartEvent = return
                  , M.appAttrMap = const theMap 
                  , M.appLiftVtyEvent = VtyEvent
                  }
        appEvent :: DownloadState -> DEvent -> T.EventM (T.Next DownloadState)
        appEvent ds@(DownloadState doneB) de = case de of
            VtyEvent e -> case e of
                V.EvKey V.KEsc [] -> M.halt ds
                V.EvKey (V.KChar 'q') _ -> M.halt ds
                V.EvKey (V.KChar 'o') [] -> M.continue $ UserInput ds (E.editor "command" (str.unlines) (Just 1) "")
                V.EvKey V.KEnter [] -> do
                    liftIO $ filepath `openBy` ""
                    M.continue ds
                ev -> M.continue ds
            UpdateFinishedSize b -> M.continue . DownloadState $ b
            DownloadFinish -> M.continue FinishedState
        appEvent FinishedState de = case de of
            VtyEvent e -> case e of
                V.EvKey (V.KChar 'q') _ -> M.halt FinishedState
                V.EvKey (V.KChar 'o') [] -> M.continue $ UserInput FinishedState (E.editor "command" (str.unlines) (Just 1) "")
                V.EvKey V.KEnter [] -> do
                    liftIO $ filepath `openBy` ""
                    M.halt FinishedState
                    -- ^ file opened, we can quit download interface now
                ev -> M.continue FinishedState
            _ -> error "received non vty event after FinishedState was reached."
        appEvent (UserInput st ed) de = case de of
            VtyEvent e -> case e of
                V.EvKey V.KEsc [] -> M.continue st
                V.EvKey V.KEnter [] -> do
                    liftIO $ filepath `openBy` concat (E.getEditContents ed)
                    case st of
                        DownloadState _ -> M.continue st
                        FinishedState -> M.halt FinishedState
                        _ -> error "unexpected state in UserInput state."
                ev -> do
                    newEd <- T.handleEvent ev ed
                    M.continue $ UserInput st newEd
            UpdateFinishedSize b -> M.continue (UserInput (DownloadState b) ed)
            DownloadFinish -> M.continue (UserInput FinishedState ed)
        theMap = A.attrMap V.defAttr [ (P.progressCompleteAttr, V.black `on` V.cyan)
                                     , (P.progressIncompleteAttr, V.black `on` V.white)
                                     , ("input box", V.black `on` V.blue)
                                     ]
        drawUI :: DownloadState -> [Widget]
        drawUI (DownloadState bytes) = [vBox [bar, note]]
            where
            bar = C.vCenter . C.hCenter $ P.progressBar Nothing (fromIntegral bytes / fromIntegral filesize)
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
                  E.renderEditor ed
    M.customMain (V.mkVty Data.Default.def) eventChan theApp initialState
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
    
download :: Text -> -- ^ url
            Text -> -- ^ filepath
            (DEvent -> IO ()) -> IO ()
download url tFilepath tell = do
    -- this implementation needs to be change
    -- since some filename contains characters that cannot be represented by String
    let
        filepath = T.unpack tFilepath
        checkFile :: IO ()
        checkFile = forever $ do
            threadDelay $ 1000000 * 1
            exist <- fileExist filepath
            when exist $ do
                s <- fileSize <$> getFileStatus filepath
                tell . UpdateFinishedSize . fromIntegral $ s
    checkerThreadID <- forkIO checkFile
    -- note: this usage of parseUrl is dangerous, exception need to be catch in the future
    req <- getUsername >>= \case
            Nothing -> parseUrl (T.unpack url)
            Just name -> getPassword >>= \case
                Nothing -> error "no password."
                Just pw -> applyBasicAuth (encodeUtf8 name) (encodeUtf8 pw) <$> parseUrl (T.unpack url)
    manager <- newManager tlsManagerSettings
    -- use http and ResumableSource in conduit for constant memory usage
    runResourceT $ do
        response <- http req manager
        let body = responseBody response
        body $$+- sinkFile filepath
        liftIO $ do
            killThread checkerThreadID
            tell DownloadFinish
    return ()

