{-# LANGUAGE OverloadedStrings #-}

module Search where

import Video

import Data.Function
import qualified Data.Text as T

search :: [Video] -> [String] -> [Video]
search vids par = filter ok vids
    where ok v = all (\tar -> (T.pack tar) `ifx` name) par
            where name = if isVid v then vidName v else fldName v
                  ifx = T.isInfixOf `on` T.toCaseFold

