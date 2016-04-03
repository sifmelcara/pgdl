{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase#-}

module DownloadInterface (downloadInterface)
where

import Debug.Trace
import qualified Data.Text as T
import Data.Text (Text)
import Networking
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import qualified Control.Concurrent.Chan as C

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.ProgressBar as P
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

import Network.HTTP.Conduit
import Control.Concurrent

import Control.Monad.Trans.Resource 
import Data.Conduit
import Control.Monad.IO.Class
import Data.Conduit.Binary
import System.Posix.Files
import System.Process
import Distribution.System
import Data.Text.Encoding

import Configure

--                                 bytes already downloaded
data DownloadState = DownloadState Integer | FinishedState

data DEvent = VtyEvent V.Event | UpdateFinishedSize Integer | DownloadFinish

-- | Maybe we should try to get file size by other method,
-- that would be more accurate and reliable.
downloadInterface :: Text -> -- ^ url
                     Text -> -- ^ file path
                     Integer -> -- ^ filesize in bytes
                     IO ()
downloadInterface url filepath filesize = do
    eventChan <- C.newChan 
    forkIO $ download url filepath (C.writeChan eventChan)
    let
        initialState :: DownloadState 
        initialState = DownloadState 0
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
                V.EvKey V.KEnter [] -> do
                    liftIO $ xdgOpen filepath
                    M.continue ds
                ev -> M.continue ds
            UpdateFinishedSize b -> M.continue . DownloadState $ b
            DownloadFinish -> M.continue FinishedState
        theMap = A.attrMap V.defAttr [ (P.progressCompleteAttr, V.black `on` V.cyan)
                                     , (P.progressIncompleteAttr, V.black `on` V.white)
                                     ]
        drawUI :: DownloadState -> [Widget]
        drawUI (DownloadState bytes) = [vBox [bar, note]]
            where
            bar = C.vCenter . C.hCenter $ P.progressBar Nothing (fromIntegral bytes / fromIntegral filesize)
            note = C.vCenter . C.hCenter . str $ unlines ["press Enter to open the file (even if its download has not finished)"]
        drawUI FinishedState = [ui]
            where
            ui = C.vCenter . C.hCenter . str $ unlines [ "Download Finished"
                                                       , "press Enter to open the file, or press 'q' to return to the file listing"
                                                       ]
    M.customMain (V.mkVty Data.Default.def) eventChan theApp initialState
    return ()

xdgOpen :: Text -> -- ^ filepath
           IO ()
xdgOpen tFilepath = do
    -- how to deal with file name that contains '\"'  ?
    case buildOS of
        OSX   -> runCommand $ "open " ++ addq filepath ++ " "
        Linux -> runCommand $ "nohup xdg-open " ++ addq filepath ++ " &>/dev/null &"
        _     -> error "don't know how to open file in this OS"
    return ()
    where
    filepath = T.unpack tFilepath
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
    -- let authReq = applyBasicAuth username password req
    manager <- newManager tlsManagerSettings
    runResourceT $ do
        response <- http req manager
        let body = responseBody response
        body $$+- sinkFile filepath
        liftIO $ do
            killThread checkerThreadID
            tell DownloadFinish
    return ()

