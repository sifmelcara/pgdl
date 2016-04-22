{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Utils 
where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text (Text)
import Text.Printf
import Data.Text.Encoding

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

terminalWidth :: Num a => a
terminalWidth = 80

-- | return the estimated display length of a Text
displayLength :: Text -> Integer
displayLength = sum . map charDisplayLen . T.unpack

-- | charDisplayLen determine a unicode character is a wide character or not
-- (a wide character occupy 2 space in the terminal)
-- this method may seem unreliable, but have no better idea.
charDisplayLen :: Char -> Integer
charDisplayLen c
    | (B.length . encodeUtf8 . T.pack $ [c]) > 1 = 2
    | otherwise = 1

-- | given a line of Text, cut it to a group of Text
-- which have 'len' display length
cutTextByDisplayLength :: Integer -> Text -> [Text]
cutTextByDisplayLength len = map T.pack . gp . T.unpack
    where
    gp :: String -> [String]
    gp = go 0 "" . map (\c -> (c, charDisplayLen c))
        where
        go :: Integer -> String -> [(Char, Integer)] -> [String]
        go _ acc []
            | null acc = []
            | otherwise = [reverse acc]
        go lsum acc str@(x:xs) 
            | lsum + snd x <= len = go (lsum + snd x) (fst x : acc) xs
            | otherwise = reverse acc : go 0 "" str


