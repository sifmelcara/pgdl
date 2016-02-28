{-# LANGUAGE OverloadedStrings #-}
module Configure
where

import qualified Data.Text as T

getUsername :: IO T.Text
getUsername = undefined

getPassword :: IO T.Text
getPassword = undefined

getServpath :: IO T.Text
getServpath = return "www.kernel.org/pub/linux/kernel/v4.x/"

getLocaldir :: IO T.Text
getLocaldir = undefined


