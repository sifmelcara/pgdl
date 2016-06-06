{-# LANGUAGE OverloadedStrings #-}

module Types
where

import Control.Applicative ((<$>))
import Text.HTML.DirectoryListing.Type
import Data.Text (Text)
import Data.Maybe
import qualified Brick.Widgets.List as L
import qualified Brick.Types as T
import Brick.Widgets.List (listSelectedL, listElementsL, listSelectedElement)
import qualified Data.Vector as V
import Data.Vector ((!), Vector)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as S
import Lens.Micro
import Control.Monad

data DNode = Directory Entry (IO [DNode]) | File Entry Text Bool
--                                                          downloaded?

