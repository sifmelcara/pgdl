{-# LANGUAGE OverloadedStrings #-}

module Beaut where

import Data.List
import qualified Data.Text as T
import Video

beaut :: Video -> T.Text
beaut vid
    | length dat < 3 = "\n" `app` str
    | otherwise      = T.intercalate "\n" [tb 4 `app` sbt, 
                                           tb 10 `app` nme, 
                                           tb 30 `app` eps
                                          ]
    where str = vidName vid
          dat = spl str
          (sbt:nme:eps:_) = dat
          tb n = T.replicate n " "
          app s1 s2 = s1 `T.append` s2
          spl = filter (\t -> T.length t /= 0) . concatMap (T.splitOn "[") . (T.splitOn "]")



