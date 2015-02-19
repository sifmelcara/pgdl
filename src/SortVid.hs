{-# LANGUAGE OverloadedStrings #-}

module SortVid (sortVid) where

import Video

import qualified Data.Text as T
import Data.List
import Data.Function

sortVid :: [Video] -> [Video]
sortVid = sortBy cmpv

cmpv :: Video -> Video -> Ordering
cmpv v1 v2
    | isVid v1 = (cmp `on` vidDate) v1 v2
    | otherwise = (cmp `on` fldDate) v1 v2

cmp :: T.Text -> T.Text -> Ordering
cmp = flip compare `on` prs
    where prs tx = [year, tranMon mon, day, time] 
            where [day, mon, year, time] = T.splitOn "-" tx
          
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

