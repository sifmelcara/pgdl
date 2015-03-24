{-# LANGUAGE OverloadedStrings #-}

module PrsVid where

import Video
import NameAlgo

import Network.HTTP.Base
import Text.HTML.TagSoup
import qualified Data.Text as T

prsHtm :: T.Text -> [Video]
prsHtm t = if null vs then [noVideo] else vs
    where vs = sortVid $ (prsVid t) ++ (prsFld t)
          noVideo = Video "No video in this folder." "" "" ""

prsVid :: T.Text -> [Video]
prsVid = map (genVid . filter (not . T.null) . map pullText) .
         filter isVidLn . map parseTags . T.lines
    where isVidLn = any isVidLnk
          isVidLnk tg
            | isTagOpen tg = any (`T.isSuffixOf` fromAttrib "href" tg) fmts
            | otherwise = False
          pullText tg
            | isTagOpen tg = fromAttrib "href" tg
            | isTagText tg = fromTagText tg
            | otherwise = ""
          genVid [lnk, _, tx] = Video {vidLink = lnk, vidName = nm, vidDate = dt, vidSize = sz}
            where dt = T.intercalate "-" . take 2 $ T.words tx
                  sz = T.pack . hsz . read . last . words . T.unpack $ tx
                  hsz :: Int -> String
                  hsz i = show (i `div` (1024*1024)) ++ "M"
                  nm = genName lnk
          genVid _ = Video "Parse Fail" "" "" ""
          fmts = [".mp4", ".avi", ".mkv"]

prsFld :: T.Text -> [Video]
prsFld = map (mkFld . filter (not . T.null) . map pullText) . filter isFldLn . map parseTags . T.lines
    where isFldLn = isFldLnk . head
          isFldLnk tg
            | isTagOpen tg = (`T.isSuffixOf` fromAttrib "href" tg) "/"
            | otherwise = False
          mkFld :: [T.Text] -> Video
          mkFld [lnk, _, tx] = Folder {fldName = nm, fldLink = lnk, fldDate = dt}
            where nm = T.init . genName $ lnk
                  dt = T.intercalate "-" . take 2 $ T.words tx
          mkFld _ = Folder "Folder Parse Fail" "" ""
          pullText tg
            | isTagOpen tg = fromAttrib "href" tg
            | isTagText tg = fromTagText tg
            | otherwise = ""

genName :: T.Text -> T.Text
genName = T.pack . urlDecode . T.unpack
