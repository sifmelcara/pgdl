{-# LANGUAGE OverloadedStrings #-}

module EntryAttrViewer (entryAttrViewer)
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as C

import Text.HTML.DirectoryListing.Type
import Types
import qualified Utils as U

entryAttrViewer :: DNode -> IO ()
entryAttrViewer (Directory entry _) = entryAttrViewer (File entry "directory no link." False)
entryAttrViewer (File entry url downloaded) =
    M.simpleMain . C.vCenter . C.hCenter . C.hLimit U.terminalWidth . C.txt $ info
    where
    info = T.unlines $
           zipWith (\describ s -> T.intercalate "\n" . 
                                  U.cutTextByDisplayLength U.terminalWidth $
                                  describ `T.append` ": " `T.append` s
                   )
           [ "visible name" 
           , "decoded name" 
           , "url" 
           , "file size" 
           , "downloaded"
           ]
           [ visibleName entry
           , decodedName entry
           , url
           , maybe "directory" (T.pack . U.friendlySize) . fileSize $ entry
           , if downloaded then "Yes" else "No"
           ]

