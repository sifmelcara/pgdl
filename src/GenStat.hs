{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GenStat (genStat) where

import Video
import TestExist

import qualified Data.Text as T

genStat :: Video -> IO T.Text
genStat (Video name _ size date) = do
    st <- toStr $ downloaded name
    return $ T.concat [ spc 3 , date
                      , spc 5 , size
                      , spc 20, st
                      , spc 30
                      ]
    where app = T.append
          spc :: Int -> T.Text
          spc i = T.replicate i " "

toStr :: IO Bool -> IO T.Text
toStr b = b >>= \case
    True -> return "Downloaded"
    _    -> return "Not Downloaded"


