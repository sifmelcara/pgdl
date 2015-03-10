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
    x == y 
        | isVid x && isVid y = vidName x == vidName y
        | isFld x && isFld y = fldName x == fldName y
        | otherwise = False

getName :: Video -> T.Text
getName (Video n _ _ _) = n
getName (Folder n _ _) = n

isVid :: Video -> Bool
isVid Video {} = True
isVid _ = False

isFld :: Video -> Bool
isFld v = not $ isVid v


attcLink :: T.Text -> Video -> Video
attcLink fnt (Video name linkt size date) = Video 
                         { vidName = name
                         , vidLink = T.pack $ fn </> link
                         , vidSize = size
                         , vidDate = date
                         }
    where fn = T.unpack fnt
          link = T.unpack linkt
attcLink fnt (Folder name linkt date) = Folder
                         { fldName = name
                         , fldLink = T.pack $ fn </> link
                         , fldDate = date
                         }
    where fn = T.unpack fnt
          link = T.unpack linkt

