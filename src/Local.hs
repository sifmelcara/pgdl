{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Local
where

import System.Directory
import System.FilePath.Posix
import Data.Text (Text)
import qualified Data.Text as T

-- | determine whether a file is in specified directory
isFileDownloaded :: Text -> -- ^ file name
                    String -> -- ^ local directory
                    IO Bool
isFileDownloaded fn path = doesFileExist $ path </> T.unpack fn
    
deleteFile :: Text -> -- ^ file name (may be a absolute path)
              IO ()
deleteFile = removeFile . T.unpack

