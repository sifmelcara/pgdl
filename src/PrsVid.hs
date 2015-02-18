{-# LANGUAGE OverloadedStrings #-}

module PrsVid where

import Video
import GenName
import SortVid

import Text.HTML.TagSoup
import qualified Data.Text as T

prsVid :: T.Text -> [Video]
prsVid = sortVid .
         map (genVid . filter (not . T.null) . map pullText) .
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
            where dt = T.intercalate " " . take 2 $ T.words tx
                  sz = T.pack . hsz . read . last . words . T.unpack $ tx
                  hsz :: Int -> String
                  hsz i = show (i `div` (1024*1024)) ++ "M"
                  nm = genName lnk
          genVid _ = Video "Parse Fail" "" "" ""
          fmts = [".mp4", ".avi", ".mkv"]

prsFld :: T.Text -> [Video]
prsFld = sortVid . map (mkFld . filter (not . T.null) . map pullText) . filter isFldLn . map parseTags . T.lines
    where isFldLn = isFldLnk . head
          isFldLnk tg
            | isTagOpen tg = (`T.isSuffixOf` fromAttrib "href" tg) "/"
            | otherwise = False
          mkFld :: [T.Text] -> Video
          mkFld [lnk, _, tx] = Folder {fldName = nm, fldLink = lnk, fldDate = dt}
            where nm = T.init . genName $ lnk
                  dt = T.intercalate "" . take 2 $ T.words tx
          pullText tg
            | isTagOpen tg = fromAttrib "href" tg
            | isTagText tg = fromTagText tg
            | otherwise = ""


