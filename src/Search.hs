{-# LANGUAGE OverloadedStrings #-}

module Search where

import Video

import Data.Function

import qualified Data.Text as T

search :: [Video] -> [String] -> [Video]
search vids par = filter pred vids
    where pred v = all (\tar -> name `T.isInfixOf` (T.pack tar)) par
            where name = vidName v

{-
search vlst param = foldl (\vs ps -> filter (targ ps . vidName) vs) vlst pt
    where pt = map T.pack param
          targ = T.isInfixOf `on` T.toUpper
-}

