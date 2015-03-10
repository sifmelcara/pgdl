{-# LANGUAGE OverloadedStrings #-}

module CrtInf where

import Video

import qualified Data.Text as T

crtInfPg :: Video -> T.Text
crtInfPg vid = T.unlines ["", "",
                          "File name: " `app` (spl . getName) vid,
                          "File size: " `app` getSize vid,
                          "File date: " `app` getDate vid,
                          "File link: " `app` (spl . getLink) vid
                         ]
    where app = T.append
          spl = T.intercalate "\n" . T.chunksOf 60 
          getSize v
            | isVid v = vidSize v
            | otherwise = "I'm a folder"
          getDate v
            | isVid v = vidDate v
            | otherwise = fldDate v
          getLink v
            | isVid v = vidLink v
            | otherwise = fldLink v

