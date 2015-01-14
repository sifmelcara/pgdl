{-# LANGUAGE OverloadedStrings #-}

module Search where

import qualified Data.Text as T
import Data.Function

search :: [(T.Text, T.Text, T.Text)] -> [String] -> [(T.Text, T.Text, T.Text)]
search vlst plst = foldl (\vs ps -> filter (targ ps . getName) vs) vlst pt
    where pt = map T.pack plst
          getName (s, _, _) = s
          targ = T.isInfixOf `on` T.toUpper


