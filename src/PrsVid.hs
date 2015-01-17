{-# LANGUAGE OverloadedStrings #-}

module PrsVid where

import Text.HTML.TagSoup
import Data.List
import Data.Function
import qualified Data.Text as T
import ConstVal
import Video

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
          genVidInf ss = Video {vidLink = ss!!0, vidName = ss!!1, vidDate = ss!!2, vidSize = ss!!3}  




