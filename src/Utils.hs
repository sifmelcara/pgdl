{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Utils 
where

import qualified Data.Text as T
import Data.Text (Text)
import Text.Printf

-- | give a file size in bytes, return pretty file size 
-- represent in KB, MB, GB or TB
friendlySize :: Integer -> String
friendlySize b
    | fb < 1024 = s2s fb ++ " B "
    | kb < 1024 = s2s kb ++ " KB"
    | mb < 1024 = s2s mb ++ " MB"
    | gb < 1024 = s2s gb ++ " GB"
    | otherwise = s2s tb ++ " TB"
    where
    fb :: Double
    fb = fromIntegral b
    kb = fb / 1024.0
    mb = kb / 1024.0
    gb = mb / 1024.0
    tb = gb / 1024.0
    s2s :: Double -> String
    s2s = printf "%6s" . (printf "%.1f" :: Double -> String)

