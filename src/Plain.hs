{-# LANGUAGE OverloadedStrings #-}

module Plain where

import Data.List
import qualified Data.Text as T

plain :: (T.Text, T.Text, T.Text) -> T.Text
plain (str, _, sz) = "\n" `app` str `app` "\n" `app` tb 50 `app` sz
    where app = T.append
          tb x = T.replicate x " "

