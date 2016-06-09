{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Utils 
where

import qualified Data.Text as T
import Data.Text (Text)
import Text.Printf
import System.IO
import qualified Graphics.Text.Width as TW

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
terminalWidth = 83

-- | charDisplayLen returns the display length of a character
charDisplayLen :: Char -> Integer
charDisplayLen c
    | w > 0 = fromIntegral w
    | otherwise = 2 -- assume unknown characters occupy 2 widths
    where
    w = TW.safeWcwidth c

-- | return the estimated display length of a Text
displayLength :: Text -> Integer
displayLength = sum . map charDisplayLen . T.unpack

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

askPassword :: IO Text
askPassword = do
    putStr "please input password: "
    hFlush stdout
    hSetEcho stdin False
    pass <- getLine
    hSetEcho stdin True
    putChar '\n'
    return $ T.pack pass

placeTextIntoRectangle :: Int -> -- ^ height
                          Int -> -- ^ width
                          Text -> Text
placeTextIntoRectangle h w t 
    | l > w = error "placeTextIntoRectangle: text too long"
    | otherwise = T.unlines $
        replicate (h `div` 2) (T.replicate w " ") ++
        [T.concat [T.replicate leftspace " ", t, T.replicate rightspace " "]] ++
        replicate (h `div` 2) (T.replicate w " ")
    where
    l = fromIntegral $ displayLength t
    leftspace = (w-l) `div` 2
    rightspace = w - leftspace - l

