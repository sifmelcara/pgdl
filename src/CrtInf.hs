{-# LANGUAGE OverloadedStrings #-}

module CrtInf where

import Video

import qualified Data.Text as T

crtInfPg :: Video -> T.Text
crtInfPg vid = T.unlines ["", "",
                          "File name: " `app` (spl . vidName) vid,
                          "File size: " `app` vidSize vid,
                          "File date: " `app` vidDate vid,
                          "File link: " `app` (spl . vidLink) vid
                         ]
    where app = T.append
          spl = T.intercalate "\n" . T.chunksOf 60 



