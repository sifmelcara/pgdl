{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GenStat (genStat, waitingBar) where

import Video
import RealWorld

import qualified Data.Text as T

genStat :: Video -> IO T.Text
genStat (Video name _ size date) = do
    st <- toStr $ downloaded name
    return $ T.concat [ spc 3 , date
                      , spc 5 , size
                      , spc 20, st
                      , spc 30
                      ]
    where spc :: Int -> T.Text
          spc i = T.replicate i " "

genStat (Folder _ _ date) = 
    return $ T.concat [ spc 3
                      , date
                      , spc 5
                      , "I'm a folder"
                      , spc 99
                      ]
    where spc i = T.replicate i " "

waitingBar :: T.Text
waitingBar = "   Waiting for data..." `T.append` T.replicate 99 " "

toStr :: IO Bool -> IO T.Text
toStr b = b >>= \case
    True -> return "Downloaded"
    _    -> return "Not Downloaded"


