{-# LANGUAGE OverloadedStrings #-}

module SortVid (sortVid) where

import Video

import qualified Data.Text as T
import Data.List
import Data.Function

sortVid :: [Video] -> [Video]
sortVid = sortBy cmp

cmp :: Video -> Video -> Ordering
cmp = flip compare `on` prs
    where prs tx = [year, tranMon mon, day, time, vidName tx] 
            where [day, mon, year, time] = concat . map (T.splitOn "-") . (T.splitOn " ") . vidDate $ tx
          
tranMon :: T.Text -> T.Text
tranMon m = case m of
    "Jan" -> "A"
    "Feb" -> "B"
    "Mar" -> "C"
    "Apr" -> "D"
    "May" -> "E"
    "Jun" -> "F"
    "Jul" -> "G"
    "Aug" -> "H"
    "Sep" -> "I"
    "Oct" -> "J"
    "Nov" -> "K"
    "Dec" -> "L"
    _     -> " "

