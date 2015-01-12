{-# LANGUAGE OverloadedStrings #-}

module Search where

import Data.List
import qualified Data.Text as T

search :: [(T.Text, T.Text, T.Text)] -> [String] -> [(T.Text, T.Text, T.Text)]
search vlst plst = foldl (\vs ps -> filter (T.isInfixOf ps . getName) vs) vlst pt
    where pt = map T.pack plst
          getName (s, _, _) = s


