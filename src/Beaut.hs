{-# LANGUAGE OverloadedStrings #-}

module Beaut where

import Data.List
import qualified Data.Text as T

beaut :: (T.Text, T.Text, T.Text) -> T.Text
beaut (str, _, sz)
    | length dat < 3 = "\n" `app` str
    | otherwise      = T.intercalate "\n" [tb 4 `app` sbt, 
                                           tb 10 `app` nme, 
                                           tb 30 `app` eps
                                          ]
    where dat =  map cuth . T.splitOn "]" $ str
          (sbt:nme:eps:_) = dat
          tb n = T.replicate n " "
          cuth str
            | T.null str = str
            | T.head str == '[' = T.tail str
            | otherwise = str
          app s1 s2 = s1 `T.append` s2



