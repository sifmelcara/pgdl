{-# LANGUAGE OverloadedStrings #-}

module PrsVid where

import Text.HTML.TagSoup
import Data.List
import Data.Function
import qualified Data.Text as T
import ConstVal

prsVid :: T.Text -> [(T.Text, T.Text, T.Text)]
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
          getInf tg
            | isNameTag tg = fromTagText tg
            | isLinkTag tg = fromAttrib "href" tg
            | isSizeTag tg = fromTagText tg
            | otherwise = ""
          genVidInf ss = (ss!!1, ss!!0, ss!!2)




