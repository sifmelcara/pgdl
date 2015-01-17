{-# LANGUAGE OverloadedStrings #-}

module Search where

import qualified Data.Text as T
import Data.Function
import Video

search :: [Video] -> [String] -> [Video]
search vlst param = foldl (\vs ps -> filter (targ ps . vidName) vs) vlst pt
    where pt = map T.pack param
          targ = T.isInfixOf `on` T.toUpper


