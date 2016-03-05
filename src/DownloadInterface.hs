{-# LANGUAGE OverloadedStrings #-}
module DownloadInterface (downloadInterface)
where

import Debug.Trace
import qualified Data.Text as T
import Data.Text (Text)
import Networking
import Control.Monad
import Control.Monad.IO.Class

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

--                                 bytes already downloaded
data DownloadState = DownloadState Integer


-- | Maybe we should try to get file size by other method,
-- that would be more accurate and reliable.
downloadInterface :: Text -> -- ^ url
                     Integer -> -- ^ filesize in bytes
                     IO ()
downloadInterface url filesize = do
    let
        initialState :: DownloadState 
        initialState = DownloadState 333
        theApp =
            M.App { M.appDraw = drawUI
                  , M.appChooseCursor = M.neverShowCursor
                  , M.appHandleEvent = appEvent
                  , M.appStartEvent = return
                  , M.appAttrMap = const theMap 
                  , M.appLiftVtyEvent = id
                  }
        appEvent :: DownloadState -> V.Event -> T.EventM (T.Next DownloadState)
        appEvent ds e = case e of
            V.EvKey V.KEsc [] -> M.halt ds
            ev -> M.continue ds
        theMap = A.attrMap V.defAttr [ (P.progressCompleteAttr, V.black `on` V.cyan)
                                     , (P.progressIncompleteAttr, V.black `on` V.white)
                                     ]
        drawUI :: DownloadState -> [Widget]
        drawUI (DownloadState bytes) = [ui]
            where
            ui = C.vCenter . C.hCenter $ P.progressBar Nothing 0.7--(fromIntegral bytes / fromIntegral filesize)
    M.defaultMain theApp initialState
    return ()

