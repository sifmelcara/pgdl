{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GenStat (genStat) where

import Video
import TestExist

import qualified Data.Text as T

genStat :: Video -> IO T.Text
genStat vid = do
    st <- toStr $ downloaded name
    return $ (spc 3) `app` date `app`
             (spc 3) `app` size `app`
             (spc 20) `app` st `app`
             (spc 30)
    where date = vidDate vid
          size = vidSize vid
          name = vidName vid
          app = T.append
          spc :: Int -> T.Text
          spc i = T.replicate i " "

toStr :: IO Bool -> IO T.Text
toStr b = b >>= \case
    True -> return "Downloaded"
    _    -> return "Not Downloaded"

