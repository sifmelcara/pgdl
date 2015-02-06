{-# LANGUAGE OverloadedStrings #-}

module PrsVid where

import Video
import GenName

import Data.List
import Data.Function
import Text.HTML.TagSoup
import qualified Data.Text as T

prsVid :: T.Text -> [Video]
prsVid = map (genVid . filter (not . T.null) . map pullText) . filter isVidLn . map parseTags . T.lines
    where isVidLn = any isVidLnk
          isVidLnk tg
            | isTagOpen tg = any (`T.isSuffixOf` (fromAttrib "href" tg)) fmts
            | otherwise = False
          pullText tg
            | isTagOpen tg = fromAttrib "href" tg
            | isTagText tg = fromTagText tg
            | otherwise = ""
          genVid [lnk, _, tx] = Video {vidLink = lnk, vidName = nm, vidDate = dt, vidSize = sz}
            where dt = T.concat . take 2 $ T.words tx
                  sz = T.pack . hsz . read . last . words . T.unpack $ tx
                  hsz :: Int -> String
                  hsz i = show (i `div` (1024*1024)) ++ "M"
                  nm = genName lnk
          genVid _ = Video "Parse Fail" "" "" ""
          fmts = [".mp4", ".avi", ".mkv"]


{-
         map (genVidInf . filter (not. T.null) . map getInf) .
         filter isVideoLine . 
         map parseTags . T.lines
    where isVideoLine = any isNameTag
          isNameTag tg
            | isTagText tg = any (`T.isSuffixOf` fromTagText tg) vdfmt
            | otherwise = False
          isLinkTag tg
            | isTagOpen tg = not. T.null $ fromAttrib "href" tg
            | otherwise = False
          isDtSzTag tg
            | isTagText tg = all (`T.isInfixOf` fromTagText tg)  ["-", ":"]
            | otherwise = False
          getInf tg
            | isNameTag tg = fromTagText tg
            | isLinkTag tg = fromAttrib "href" tg
            | isDtSzTag tg = $ fromTagText tg
            | otherwise = ""
          genVidInf [lnk, nm, dt, sz] = Video {vidLink = lnk, vidName = nm, vidDate = dt, vidSize = sz}  
          genVidInf _ = Video {vidLink = T.empty, vidName = "parse error", vidDate = "", vidSize = ""}
          vdfmt = [".avi", ".mp4", ".mkv"]



-}
