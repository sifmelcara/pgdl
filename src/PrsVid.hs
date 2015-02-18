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
prsFld = map (mkFld . head) . map (filter isTagOpen) . filter isFldLn . map parseTags . T.lines
    where isFldLn = any isFldLnk
          isFldLnk tg
            | isTagOpen tg = (`T.isSuffixOf` fromAttrib "href" tg) "/"
            | otherwise = False
          mkFld :: Tag T.Text -> Video
          mkFld tg = Folder {fldName = T.init . genName $ tx, fldLink = tx}
            where tx = fromAttrib "href" tg

