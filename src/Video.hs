{-# LANGUAGE OverloadedStrings #-}

module Video where

import System.FilePath
import qualified Data.Text as T

data Video = Video { vidName :: T.Text
                   , vidLink :: T.Text
                   , vidSize :: T.Text
                   , vidDate :: T.Text
                   } |
             Folder { fldName :: T.Text
                    , fldLink :: T.Text
                    , fldDate :: T.Text
                    }

instance Eq Video where
    x == y = vidName x == vidName y
    
isVid :: Video -> Bool
isVid Video {} = True
isVid _ = False

isFld :: Video -> Bool
isFld v = not $ isVid v


attcLink :: T.Text -> Video -> Video
attcLink fnt vid = Video { vidName = name
                         , vidLink = T.pack $ fn </> link
                         , vidSize = size
                         , vidDate = date
                         }
    where fn = T.unpack fnt
          (Video name linkt size date) = vid
          link = T.unpack linkt

