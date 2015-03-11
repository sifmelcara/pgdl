{-# LANGUAGE OverloadedStrings #-}

module NameAlgo where

import Video

import Data.Function
import Data.Array
import qualified Data.Text as T

-- | the video name parsing is only for specific type of videos...( [subtitle][video name][episode].... )
-- for most filename, the parsing function will just return the video name itself.
beaut :: Video -> T.Text
beaut (Video str _ _ _) = beautT str
beaut (Folder str _ _)  = beautT str

beautT :: T.Text -> T.Text
beautT str = case cutName str of
    Left x -> "\n" `apd` x
    Right (sbt:nam:eps:_) -> T.unlines  [ ws 4  `apd` sbt
                                        , ws 10 `apd` nam
                                        , ws 30 `apd` eps
                                        ]
    _ -> error "something wrong in video name parsing function"
    where apd = T.append
          ws t = T.replicate t " "

-- | cut the name into pisces
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

-- | return the edit distance of two string
editDis :: T.Text -> T.Text -> Int
editDis t1 t2 = dp!(len1, len2)
    where (s1, s2) = (T.unpack t1, T.unpack t2)
          (len1, len2) = (length s1, length s2)
          a1 = listArray (1, len1) s1
          a2 = listArray (1, len2) s2
          bnd = ((0, 0), (len1, len2))
          dp = listArray bnd [go i j | (i, j) <- range bnd]
          go i 0 = i
          go 0 j = j
          go i j = minimum [ dp!(i, j-1) + 1
                           , dp!(i-1, j) + 1
                           , dp!(i-1, j-1) + cost
                           ]
            where cost = if a1!i == a2!j then 0 else 1

-- | caculate whether the edit distance of two video
-- name is less than half of the first video's length
isAlike :: Video -> Video -> Bool
isAlike vf1 vf2 = dis <= lim
    where (n1, n2) = (getName vf1, getName vf2)
          (e1, e2) = (cutName n1, cutName n2)
          vn1 = either id (!!1) e1
          vn2 = either id (!!1) e2
          -- vn1 and vn2 are the video name without subtitle or episode if being parsed.
          lim = length (T.unpack vn1) `div` 2
          dis = editDis vn1 vn2

search :: [Video] ->  -- ^ video list
          [String] -> -- ^ a list of target string
          [Video]
search vids par = filter ok vids
    where ok v = all (\tar -> T.pack tar `ifx` name) par
            where name = if isVid v then vidName v else fldName v
                  ifx = T.isInfixOf `on` T.toCaseFold

