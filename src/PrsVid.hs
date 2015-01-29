{-# LANGUAGE OverloadedStrings #-}

module PrsVid where

import Video

import Data.List
import Data.Function
import Text.HTML.TagSoup
import qualified Data.Text as T

prsVid :: T.Text -> [Video]
prsVid = map (genVidInf . filter (not. T.null) . map getInf) .
         filter isVideoLine . 
         map parseTags . T.lines
    where isVideoLine = any isNameTag
          isNameTag tg
            | isTagText tg = any (`T.isSuffixOf` fromTagText tg) vdfmt
            | otherwise = False
          isLinkTag tg
            | isTagOpen tg = not. T.null $ fromAttrib "href" tg
            | otherwise = False
          isSizeTag tg
            | isTagText tg = any (`T.isSuffixOf` fromTagText tg) ["K", "M", "G"]
            | otherwise = False
          isDateTag tg
            | isTagText tg = all (`T.isInfixOf` fromTagText tg)  ["-", ":"]
            | otherwise = False
          getInf tg
            | isNameTag tg = fromTagText tg
            | isLinkTag tg = fromAttrib "href" tg
            | isSizeTag tg = fromTagText tg
            | isDateTag tg = fromTagText tg
            | otherwise = ""
          genVidInf [lnk, nm, dt, sz] = Video {vidLink = lnk, vidName = nm, vidDate = dt, vidSize = sz}  
          getVidInf _ = Video {vidLink = T.empty, vidName = "parse error", vidDate = "", vidSize = ""}
          vdfmt = [".avi", ".mp4", ".mkv"]




