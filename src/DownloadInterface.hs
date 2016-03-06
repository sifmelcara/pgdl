{-# LANGUAGE OverloadedStrings #-}
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

--                                 bytes already downloaded
data DownloadState = DownloadState Integer

data DEvent = VtyEvent V.Event | UpdateFinishedSize Integer | DownloadFinish

-- | Maybe we should try to get file size by other method,
-- that would be more accurate and reliable.
downloadInterface :: Text -> -- ^ url
                     Integer -> -- ^ filesize in bytes
                     IO ()
downloadInterface url filesize = do
    eventChan <- C.newChan 
    forkIO $ download url (C.writeChan eventChan)
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
                ev -> M.continue ds
            UpdateFinishedSize b -> M.continue . DownloadState $ b
            DownloadFinish -> M.halt ds
        theMap = A.attrMap V.defAttr [ (P.progressCompleteAttr, V.black `on` V.cyan)
                                     , (P.progressIncompleteAttr, V.black `on` V.white)
                                     ]
        drawUI :: DownloadState -> [Widget]
        drawUI (DownloadState bytes) = [ui]
            where
            ui = C.vCenter . C.hCenter $ P.progressBar Nothing (fromIntegral bytes / fromIntegral filesize)
    M.customMain (V.mkVty Data.Default.def) eventChan theApp initialState
    return ()


download :: Text -> (DEvent -> IO ()) -> IO ()
download url tell = do
    -- this implementation needs to be change
    -- since some filename contains characters that cannot be represented by String
    let
        filepath = "testFile"
        checkFile :: IO ()
        checkFile = forever $ do
            threadDelay $ 1000000 * 1
            exist <- fileExist filepath
            when exist $ do
                s <- fileSize <$> getFileStatus filepath
                tell . UpdateFinishedSize . fromIntegral $ s
    checkerThreadID <- forkIO checkFile
    req <- parseUrl (T.unpack url)
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

