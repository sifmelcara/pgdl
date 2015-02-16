{-# LANGUAGE OverloadedStrings #-}

module Video where

import qualified Data.Text as T

data Video = Video { vidName :: T.Text
                   , vidLink :: T.Text
                   , vidSize :: T.Text
                   , vidDate :: T.Text
                   } |
             Folder { fldName :: T.Text
                    , fldLink :: T.Text
                    }

instance Eq Video where
    x == y = vidName x == vidName y
    

