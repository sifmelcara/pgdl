{-# LANGUAGE OverloadedStrings #-}

module Beaut where

import Video

import qualified Data.Text as T

-- | the video name parsing is only for specific type of videos...( [subtitle][video name][episode].... )
-- for most filename, the parsing function will just return the video name itself.

beaut :: Video -> T.Text
beaut (Video str _ _ _) = beautT str
beaut (Folder str _ _)  = beautT str

beautT :: T.Text -> T.Text
beautT str = case cutName str of
    Left x -> "\n" `apd` x
    Right [sbt, nam, eps] -> T.unlines $ [ ws 4  `apd` sbt
                                         , ws 10 `apd` nam
                                         , ws 30 `apd` eps
                                         ]
    where apd = T.append
          ws t = T.replicate t " "

cutName :: T.Text -> Either T.Text [T.Text]
cutName str
    | length dat < 3 = Left str
      -- parsing failure
    | otherwise      = Right [sbtitle, name, episode]
      -- ok.
    where (sbtitle:name:episode:_) = dat
          dat = filter (not . T.null) . getTags ["[", "]"] $ [str]
          getTags :: [T.Text] -> -- ^ the delimiters
                     [T.Text] -> -- ^ initial patterns
                     [T.Text]
          getTags dl s = foldl (\ls d -> concatMap (T.splitOn d) ls) s dl
    

